#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
bootstrap.py - bootstrapping utility for cabal-install.

This utility is only intended for use in building cabal-install
on a new platform. If you have an otherwise functional cabal-install
please rather run `cabal v2-install .`.

Usage:

  1. On a system with functional cabal-install, use `cabal v2-configure` to
     compute an install plan for the same GHC version that you will be using to
     bootstrap.

  2. Build a dependency description file (bootstrap-deps.json) from this
     install plan by running `bootstrap.py --extract-plan`.

  3. Copy bootstrap-deps.json to the bootstrapping environment.

  4. On the system you are bootstrapping, run `bootstrap.py -d
     bootstrap-deps.json`
"""

from enum import Enum
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
from textwrap import dedent
from typing import Set, Optional, Dict, List, Tuple, \
                   NewType, BinaryIO, NamedTuple, TypeVar

PACKAGES = Path('packages')
PKG_DB = PACKAGES / 'packages.conf'
BOOTSTRAP_DEPS_JSON = Path('bootstrap-deps.json')

PackageName = NewType('PackageName', str)
Version = NewType('Version', str)

class PackageSource(Enum):
    HACKAGE = 'hackage'
    PREEXISTING = 'pre-existing'
    LOCAL = 'local'

BootstrapDep = NamedTuple('BootstrapDep', [
    ('package', PackageName),
    ('version', Version),
    ('source', PackageSource),
    # only valid when source == HACKAGE
    ('revision', Optional[int]),
    ('sha256', Optional[bytes]),
])

class Compiler:
    def __init__(self, ghc_path: Path):
        if not ghc_path.is_file():
            raise TypeError(f'GHC {ghc_path} is not a file')

        self.ghc_path = ghc_path
        self.ghc_pkg_path = self._find_ghc_pkg()
        self.version = self._get_ghc_info()['Project version']

    def _get_ghc_info(self) -> Dict[str,str]:
        from ast import literal_eval
        p = subprocess.run([self.ghc_path, '--info'], capture_output=True, check=True)
        return dict(literal_eval(p.stdout))

    def _find_ghc_pkg(self) -> Path:
        info = self._get_ghc_info()
        lib_dir = Path(info['LibDir'])
        ghc_pkg = lib_dir / 'bin' / 'ghc-pkg'
        if not ghc_pkg.is_file():
            raise TypeError(f'ghc-pkg {ghc_pkg} is not a file')
        return ghc_pkg

class BadTarball(Exception):
    def __init__(self, path: Path, expected_sha256: bytes, found_sha256: bytes):
        self.path = path
        self.expected_sha256 = expected_sha256
        self.found_sha256 = found_sha256

    def __str__(self):
        return '\n'.join([
            f'Bad tarball hash: {str(self.path)}',
            f'  expected: {self.expected_sha256}',
            f'  found:    {self.found_sha256}',
        ])

def package_url(package: PackageName, version: Version) -> str:
    return f'https://hackage.haskell.org/package/{package}-{version}/{package}-{version}.tar.gz'

def package_cabal_url(package: PackageName, version: Version, revision: int) -> str:
    return f'https://hackage.haskell.org/package/{package}-{version}/revision/{revision}.cabal'

def fetch_package(package: PackageName,
                  version: Version,
                  revision: Optional[int],
                  sha256: bytes
                  ) -> Path:
    import urllib.request

    # Download source distribution
    out = PACKAGES / (f'{package}-{version}.tar.gz')
    if not out.exists():
        print(f'Fetching {package}-{version}...')
        out.parent.mkdir(parents=True, exist_ok=True)
        url = package_url(package, version)
        with urllib.request.urlopen(url) as resp:
            shutil.copyfileobj(resp, out.open('wb'))

    # Download revised cabal file
    cabal_file = PACKAGES / f'{package}.cabal'
    if revision is not None and not cabal_file.exists():
        url = package_cabal_url(package, version, revision)
        with urllib.request.urlopen(url) as resp:
            shutil.copyfileobj(resp, cabal_file.open('wb'))

    h = hash_file(hashlib.sha256(), open(out, 'rb'))
    if sha256 != h:
        raise BadTarball(out, sha256, h)

    return out

def read_bootstrap_deps(path: Path) -> List[BootstrapDep]:
    deps = json.load(path.open())
    def from_json(o: dict) -> BootstrapDep:
        o['source'] = PackageSource(o['source'])
        return BootstrapDep(**o)

    return [from_json(dep) for dep in deps]

def write_bootstrap_deps(deps: List[BootstrapDep]):
    def to_json(dep: BootstrapDep) -> object:
        return {
            'package': dep.package,
            'version': dep.version,
            'source': dep.source.value,
            'revision': dep.revision,
            'sha256': dep.sha256
        }

    json.dump([to_json(dep) for dep in deps],
              BOOTSTRAP_DEPS_JSON.open('w'),
              indent=2)

def install_dep(dep: BootstrapDep, ghc: Compiler) -> None:
    if dep.source == PackageSource.PREEXISTING:
        # We expect it to be in the compiler's bootstrap package set
        subprocess.run([str(ghc.ghc_pkg_path), 'describe', f'{dep.package}-{dep.version}'],
                       check=True, stdout=subprocess.DEVNULL)
        print(f'Using {dep.package}-{dep.version} from GHC...')
        return

    elif dep.source == PackageSource.HACKAGE:
        assert dep.sha256 is not None
        tarball = fetch_package(dep.package, dep.version, dep.revision, dep.sha256)
        subprocess.run(['tar', 'xf', tarball.resolve()],
                       cwd=PACKAGES, check=True)
        sdist_dir = PACKAGES / f'{dep.package}-{dep.version}'

        # Update cabal file with revision
        if dep.revision is not None:
            shutil.copyfile(PACKAGES / f'{dep.package}.cabal',
                            sdist_dir / f'{dep.package}.cabal')

    elif dep.source == PackageSource.LOCAL:
        if dep.package == 'Cabal':
            sdist_dir = Path('Cabal').resolve()
        elif dep.package == 'cabal-install':
            sdist_dir = Path('cabal-install').resolve()
        else:
            raise ValueError(f'Unknown local package {dep.package}')

    install_sdist(sdist_dir, ghc)

def install_sdist(sdist_dir: Path, ghc: Compiler):
    prefix = (PACKAGES / 'tmp').resolve()
    configure_args = [
        f'--package-db={PKG_DB.resolve()}',
        f'--prefix={prefix}',
        f'--with-compiler={ghc.ghc_path}',
        f'--with-hc-pkg={ghc.ghc_pkg_path}',
    ]

    def check_call(args: List[str]) -> None:
        subprocess.run(args, cwd=sdist_dir, check=True)

    check_call([str(ghc.ghc_path), '--make', 'Setup'])
    check_call(['./Setup', 'configure'] + configure_args)
    check_call(['./Setup', 'build'])
    check_call(['./Setup', 'install'])

def hash_file(h, f: BinaryIO) -> bytes:
    while True:
        d = f.read(1024)
        if len(d) == 0:
            return h.hexdigest()

        h.update(d)


# Cabal plan.json representation
UnitId = NewType('UnitId', str)
PlanUnit = NewType('PlanUnit', dict)

def read_plan(project_dir: Path) -> Dict[UnitId, PlanUnit]:
    path = project_dir / 'dist-newstyle' / 'cache' / 'plan.json'
    plan = json.load(path.open('rb'))
    return {
        UnitId(c['id']): PlanUnit(c)
        for c in plan['install-plan']
    }

def extract_plan() -> List[BootstrapDep]:
    units = read_plan(Path('.'))
    target_unit = [
        unit
        for unit in units.values()
        if unit['pkg-name'] == 'cabal-install'
        if unit['component-name'] == 'exe:cabal'
    ][0]

    def unit_to_bootstrap_dep(unit: PlanUnit) -> BootstrapDep:
        if 'pkg-src' in unit \
                and unit['pkg-src']['type'] == 'local':
            source = PackageSource.LOCAL
        elif unit['type'] == 'configured':
            source = PackageSource.HACKAGE
        elif unit['type'] == 'pre-existing':
            source = PackageSource.PREEXISTING

        return BootstrapDep(package = unit['pkg-name'],
                            version = unit['pkg-version'],
                            source = source,
                            revision = None,
                            sha256 = unit.get('pkg-src-sha256'))

    def unit_ids_deps(unit_ids: List[UnitId]) -> List[BootstrapDep]:
        deps = [] # type: List[BootstrapDep]
        for unit_id in unit_ids:
            unit = units[unit_id]
            deps += unit_deps(unit)
            deps.append(unit_to_bootstrap_dep(unit))

        return deps

    def unit_deps(unit: PlanUnit) -> List[BootstrapDep]:
        if unit['type'] == 'pre-existing':
            return []

        deps = [] # type: List[BootstrapDep]
        if 'components' in unit:
            for comp_name, comp in unit['components'].items():
                deps += unit_ids_deps(comp['depends'])
        if 'depends' in unit:
            deps += unit_ids_deps(unit['depends'])

        return deps

    return remove_duplicates(unit_deps(target_unit) + [unit_to_bootstrap_dep(target_unit)])

a = TypeVar('a')

def remove_duplicates(xs: List[a]) -> List[a]:
    # it's easier to build lists and remove duplicates later than
    # to implement an order-preserving set.
    out = [] # type: List[a]
    for x in xs:
        if x not in out:
            out.append(x)

    return out

def bootstrap(deps: List[BootstrapDep], ghc: Compiler) -> None:
    if not PKG_DB.exists():
        print(f'Creating package database {PKG_DB}')
        PKG_DB.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run([ghc.ghc_pkg_path, 'init', PKG_DB])

    for dep in deps:
        install_dep(dep, ghc)

def main() -> None:
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--extract-plan', action='store_true',
                        help='generate bootstrap-deps.json from plan.json')
    parser.add_argument('-d', '--deps', type=Path, default='bootstrap-deps.json',
                        help='bootstrap dependency file')
    parser.add_argument('-w', '--with-compiler', type=Path, default='ghc',
                        help='path to GHC')
    args = parser.parse_args()

    if args.extract_plan:
        deps = extract_plan()
        write_bootstrap_deps(deps)
        print(f'dependencies written to {BOOTSTRAP_DEPS_JSON}')
    else:
        print(dedent("""
        DO NOT use this script if you have another recent cabal-install available.
        This script is intended only for bootstrapping cabal-install on new
        architectures.
        """))

        ghc = Compiler(args.with_compiler)
        deps = read_bootstrap_deps(args.deps)
        bootstrap(deps, ghc)
        cabal_path = (PACKAGES / 'tmp' / 'bin' / 'cabal').resolve()

        print(dedent(f'''
            Bootstrapping finished!

            The resulting cabal-install executable can be found at

                {cabal_path}

            You now should use this to build a full cabal-install distribution
            using v2-build.
        '''))

if __name__ == '__main__':
    main()
