module Main (main) where

import Control.Monad      (forM_)
import Data.Function      (on)
import Data.Foldable      (for_)
import Data.Maybe         (fromMaybe, listToMaybe)
import Data.String        (fromString)
import Data.List          (intercalate, nubBy, sortBy)
import Data.Traversable   (for)
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.FilePath    ((</>))
import System.IO          (hPutStrLn, stderr)

import qualified Data.Text as T
import qualified Cabal.Index                    as I
import qualified Cabal.Plan                     as P
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import qualified Distribution.Types.PackageName as C
import qualified Distribution.Types.Version as C
import qualified Topograph                      as TG

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

printForBsdPort :: [Dep] -> IO ()
printForBsdPort deps = do
  let cleanedDeps = nubBy ((==) `on` depPackageName)
                    $ sortBy (compare `on` depPackageName) deps
  forM_ cleanedDeps $ \dep -> do
            let P.PkgName name = depPackageName dep
                P.Ver v = depVersion dep
                ver = intercalate "." (map show v)
                rev = show $ fromMaybe 0 $ depRevision dep
            putStrLn $ intercalate "\t" ["", T.unpack name, ver, rev, "\\"]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [pkg, exe, path] -> do
            let unit = (P.PkgName (fromString pkg), P.CompNameExe (fromString exe))
            x <- main1 unit (path </> "dist-newstyle/cache/plan.json")
            printForBsdPort x
        _    -> die "Usage: pkg_id exe_id cabal_config_path"

main1 :: (P.PkgName, P.CompName) -> FilePath -> IO [Dep]
main1 unit planPath = do
    meta <- I.cachedHackageMetadata
    plan <- P.decodePlanJson planPath
    main2 unit meta plan

main2 :: (P.PkgName, P.CompName) -> Map.Map C.PackageName I.PackageInfo -> P.PlanJson -> IO [Dep]
main2 unitId meta plan = do
    mapM_ (info . show) (Map.keys $ P.pjUnits plan)

    let diagExe = T.unpack p ++ ":exe:" ++ T.unpack e
        (P.PkgName p, P.CompNameExe e) = unitId

    -- find the unit to build
    (programUid, programUnit) <- case findGivenExe unitId plan of
        Just x  -> return x
        Nothing -> die $ "Cannot find " ++ diagExe ++ " unit"

    info $ diagExe ++ " unit " ++ show programUid
    info $ show programUnit

    -- BFS from program unit, getting all dependencies
    units <- bfs plan programUnit

    info $ "Unit order:"
    for_ units $ \unit -> do
        info $ " - " ++ show (P.uId unit)

    deps <- fmap concat $ for units $ \unit -> do
        let P.PkgId pkgname@(P.PkgName tpkgname) ver@(P.Ver verdigits) = P.uPId unit

        let uid = P.uId unit

        let cpkgname :: C.PackageName
            cpkgname = C.mkPackageName (T.unpack tpkgname)

        let cversion :: C.Version
            cversion = C.mkVersion verdigits

        case P.uType unit of
            P.UnitTypeBuiltin -> return []
            _ -> do
                rev <- case P.uSha256 unit of
                    Just _  -> do
                        pkgInfo <- maybe (die $ "Cannot find " ++ show uid ++ " package metadata") return $
                            Map.lookup cpkgname meta
                        relInfo <- maybe (die $ "Cannot find " ++ show uid ++ " version metadata") return $
                            Map.lookup cversion $ I.piVersions pkgInfo

                        return $
                            Just $ fromIntegral (I.riRevision relInfo)

                    Nothing -> case P.uType unit of
                        P.UnitTypeLocal   -> return Nothing
                        t                 -> die $ "Unit of wrong type " ++ show uid ++ " " ++ show t

                return $ [Dep
                    { depPackageName = pkgname
                    , depVersion     = ver
                    , depRevision    = rev
                    }]

    return deps

bfs :: P.PlanJson -> P.Unit -> IO [P.Unit]
bfs plan unit0 = do
    uids <- either (\loop -> die $ "Loop in install-plan " ++ show loop) id $ TG.runG am $ \g -> do
        v <- maybe (die $ "Cannot find " <> show unit0 <> " unit in topograph") return $
            TG.gToVertex g $ P.uId unit0

        let t = TG.dfs g v

        return $ map (TG.gFromVertex g) $
            -- nub and sort
            reverse $ Set.toList $ Set.fromList $ concat t

    for uids $ \uid -> lookupUnit units uid

  where
    am :: Map.Map P.UnitId (Set.Set P.UnitId)
    am = fmap (foldMap P.ciLibDeps . P.uComps) units

    units = P.pjUnits plan

lookupUnit :: Map.Map P.UnitId P.Unit -> P.UnitId -> IO P.Unit
lookupUnit units uid
    = maybe (die $ "Cannot find unit " ++ show uid) return
    $ Map.lookup uid units

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Dep = Dep
    { depPackageName :: P.PkgName
    , depVersion     :: P.Ver
    , depRevision    :: Maybe Int
    }
  deriving (Show)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

info :: String -> IO ()
info msg = hPutStrLn stderr $ "INFO: " ++ msg

die :: String -> IO a
die msg = do
    hPutStrLn stderr msg
    exitFailure

-------------------------------------------------------------------------------
-- Pure bits
-------------------------------------------------------------------------------

findGivenExe :: (P.PkgName, P.CompName) -> P.PlanJson -> Maybe (P.UnitId, P.Unit)
findGivenExe (pkg, exe) plan = listToMaybe
    [ (uid, unit {P.uComps = Map.singleton exe exeValue} )
    | (uid, unit) <- Map.toList (P.pjUnits plan)
    , let P.PkgId pkgname _ = P.uPId unit
    , pkgname == pkg
    , Just exeValue <- [Map.lookup exe (P.uComps unit)]
    ]
