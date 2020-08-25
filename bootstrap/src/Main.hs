module Main (main) where

import Control.Monad      (forM_, when)
import Data.Either        (partitionEithers)
import Data.Function      (on)
import Data.Foldable      (for_, traverse_)
import Data.Maybe         (fromMaybe, listToMaybe)
import Data.String        (fromString)
import Data.List          (intercalate, nubBy, sortBy)
import Data.Traversable   (for)
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

import qualified Data.Text as T
import qualified Cabal.Index                    as I
import qualified Cabal.Plan                     as P
import qualified Data.Aeson                     as A
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import qualified Distribution.Types.PackageName as C
import qualified Distribution.Types.Version as C
import qualified Topograph                      as TG

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

cabalId :: (P.PkgName, P.CompName)
cabalId = ( P.PkgName (fromString "cabal-install")
          , P.CompNameExe (fromString "cabal")
          )

shellCheckId :: (P.PkgName, P.CompName)
shellCheckId = ( P.PkgName (fromString "ShellCheck")
               , P.CompNameExe (fromString "shellcheck")
               )


darcsId :: (P.PkgName, P.CompName)
darcsId = ( P.PkgName (fromString "darcs")
          , P.CompNameExe (fromString "darcs")
          )

xmonadId :: (P.PkgName, P.CompName)
xmonadId = ( P.PkgName (fromString "xmonad")
          , P.CompNameExe (fromString "xmonad")
          )

xmobarId :: (P.PkgName, P.CompName)
xmobarId = ( P.PkgName (fromString "xmobar")
          , P.CompNameExe (fromString "xmobar")
          )

hasktagsId :: (P.PkgName, P.CompName)
hasktagsId = ( P.PkgName (fromString "hasktags")
          , P.CompNameExe (fromString "hasktags")
          )

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

printShellCheck :: IO ()
printShellCheck = do
  x <- main1 shellCheckId "/home/greg/s/ShellCheck-0.7.1/ghcver.plan.json"
  printForBsdPort $ resDependencies x

printDarcs :: IO ()
printDarcs = do
  x <- main1 darcsId "/home/greg/s/darcs-2.16.2/dist-newstyle/cache/plan.json"
  printForBsdPort $ resDependencies x

printXmonad :: IO ()
printXmonad = do
  x <- main1 xmonadId "/home/greg/s/xmonad-0.15/dist-newstyle/cache/plan.json"
  printForBsdPort $ resDependencies x

printXmobar :: IO ()
printXmobar = do
  x <- main1 xmobarId "/home/greg/s/xmobar-0.36/dist-newstyle/cache/plan.json"
  printForBsdPort $ resDependencies x

printHasktags :: IO ()
printHasktags = do
  x <- main1 hasktagsId "/home/greg/s/hasktags-0.71.2/dist-newstyle/cache/plan.json"
  printForBsdPort $ resDependencies x

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp] -> main1 cabalId fp >>= LBS.putStr . A.encode
        _    -> die "Usage: cabal-bootstrap-gen plan.json"

main1 :: (P.PkgName, P.CompName) -> FilePath -> IO Result
main1 unit planPath = do
    meta <- I.cachedHackageMetadata
    plan <- P.decodePlanJson planPath
    main2 unit meta plan

main2 :: (P.PkgName, P.CompName) -> Map.Map C.PackageName I.PackageInfo -> P.PlanJson -> IO Result
main2 unitId meta plan = do
    mapM_ (info . show) (Map.keys $ P.pjUnits plan)

    let diagExe = T.unpack p ++ ":exe:" ++ T.unpack e
        (P.PkgName p, P.CompNameExe e) = unitId

    -- find the unit to build
    (cabalUid, cabalUnit) <- case findGivenExe unitId plan of
        Just x  -> return x
        Nothing -> die $ "Cannot find " ++ diagExe ++ " unit"

    info $ diagExe ++ " unit " ++ show cabalUid
    info $ show cabalUnit

    -- BFS from cabal unit, getting all dependencies
    units <- bfs plan cabalUnit

    info $ "Unit order:"
    for_ units $ \unit -> do
        info $ " - " ++ show (P.uId unit)

    (builtin, deps) <- fmap partitionEithers $ for units $ \unit -> do
        let P.PkgId pkgname@(P.PkgName tpkgname) ver@(P.Ver verdigits) = P.uPId unit

        let uid = P.uId unit

        let cpkgname :: C.PackageName
            cpkgname = C.mkPackageName (T.unpack tpkgname)

        let cversion :: C.Version
            cversion = C.mkVersion verdigits

        case P.uType unit of
            P.UnitTypeBuiltin ->
                return $ Left Builtin
                  { builtinPackageName = pkgname
                  , builtinVersion     = ver
                  }
            
            _ -> do
                (src, rev, revhash) <- case P.uSha256 unit of
                    Just _  -> do
                        pkgInfo <- maybe (die $ "Cannot find " ++ show uid ++ " package metadata") return $
                            Map.lookup cpkgname meta
                        relInfo <- maybe (die $ "Cannot find " ++ show uid ++ " version metadata") return $
                            Map.lookup cversion $ I.piVersions pkgInfo

                        return
                            ( Hackage
                            , Just $ fromIntegral (I.riRevision relInfo)
                            , P.sha256FromByteString $ I.getSHA256 $ I.riCabal relInfo
                            )

                    Nothing -> case P.uType unit of
                        P.UnitTypeLocal   -> return (Local, Nothing, Nothing)
                        t                 -> die $ "Unit of wrong type " ++ show uid ++ " " ++ show t

                return $ Right Dep
                    { depPackageName = pkgname
                    , depVersion     = ver
                    , depSource      = src
                    , depSrcHash     = P.uSha256 unit
                    , depRevision    = rev
                    , depRevHash     = revhash
                    , depFlags       =
                        [ (if fval then "+" else "-") ++ T.unpack fname
                        | (P.FlagName fname, fval) <- Map.toList (P.uFlags unit)
                        ]
                    }

    return $ Result
        { resBuiltin      = builtin
        , resDependencies = deps
        }

bfs :: P.PlanJson -> P.Unit -> IO [P.Unit]
bfs plan unit0 = do
    uids <- either (\loop -> die $ "Loop in install-plan " ++ show loop) id $ TG.runG am $ \g -> do
        v <- maybe (die $ "Cannot find " <> show unit0 <> " unit in topograph") return $
            TG.gToVertex g $ P.uId unit0

        let t = TG.dfs g v

        return $ map (TG.gFromVertex g) $
            -- nub and sort
            reverse $ Set.toList $ Set.fromList $ concat t

    for uids $ \uid -> do
        unit <- lookupUnit units uid
        forM_ (Map.toList (P.uComps unit)) $ \(_, compinfo) ->
            checkExeDeps uid (P.pjUnits plan) (P.ciExeDeps compinfo)
        return unit

  where
    am :: Map.Map P.UnitId (Set.Set P.UnitId)
    am = fmap (foldMap P.ciLibDeps . P.uComps) units

    units = P.pjUnits plan

checkExeDeps :: P.UnitId -> Map.Map P.UnitId P.Unit -> Set.Set P.UnitId -> IO ()
checkExeDeps pkgUid units = traverse_ check . Set.toList where
    check uid = do
        unit <- lookupUnit units uid
        let P.PkgId pkgname _ = P.uPId unit
        when (pkgname /= P.PkgName (fromString "hsc2hs")) $ do
            die $ "unit " ++ show pkgUid ++ " depends on executable " ++ show uid

lookupUnit :: Map.Map P.UnitId P.Unit -> P.UnitId -> IO P.Unit
lookupUnit units uid
    = maybe (die $ "Cannot find unit " ++ show uid) return
    $ Map.lookup uid units

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Result = Result
    { resBuiltin      :: [Builtin]
    , resDependencies :: [Dep]
    }
  deriving (Show)

data Builtin = Builtin
    { builtinPackageName :: P.PkgName
    , builtinVersion     :: P.Ver
    }
  deriving (Show)

data Dep = Dep
    { depPackageName :: P.PkgName
    , depVersion     :: P.Ver
    , depSource      :: SrcType
    , depSrcHash     :: Maybe P.Sha256
    , depRevision    :: Maybe Int
    , depRevHash     :: Maybe P.Sha256
    , depFlags       :: [String]
    }
  deriving (Show)

data SrcType
    = Hackage
    | Local
  deriving (Show)

instance A.ToJSON Result where
    toJSON res = A.object
        [ fromString "builtin"      A..= resBuiltin res
        , fromString "dependencies" A..= resDependencies res
        ]

instance A.ToJSON Builtin where
    toJSON b = A.object
        [ fromString "package"      A..= builtinPackageName b
        , fromString "version"      A..= builtinVersion b
        ]

instance A.ToJSON Dep where
    toJSON dep = A.object
        [ fromString "package"      A..= depPackageName dep
        , fromString "version"      A..= depVersion dep
        , fromString "source"       A..= depSource dep
        , fromString "src_sha256"   A..= depSrcHash dep
        , fromString "revision"     A..= depRevision dep
        , fromString "cabal_sha256" A..= depRevHash dep
        , fromString "flags"        A..= depFlags dep
        ]

instance A.ToJSON SrcType where
    toJSON Hackage     = fromString "hackage"
    toJSON Local       = fromString "local"

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
