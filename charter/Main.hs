{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import           Lens.Family
import qualified System.Console.GetOpt as Opt
import qualified System.Environment as Sys
import qualified System.Exit as Sys

import qualified Charter as C

data Option
  = AddBinary T.Text
  | SetCategory T.Text
  | SetSynopsis T.Text
  | SetDescription T.Text
  | SetLicense T.Text
  | SetRoot T.Text
  | AddMod T.Text
  | AddDep T.Text
  | AddUsualDeps
    deriving (Eq, Show, Ord)

options :: [Opt.OptDescr Option]
options =
  [ Opt.Option ['b'] ["bin"]
    (Opt.ReqArg (AddBinary . T.pack) "PROGRAM NAME")
    "Add another binary target to this Cabal file"
  , Opt.Option ['m'] ["module"]
    (Opt.ReqArg (AddMod . T.pack) "MODULE NAME")
    "Add another library module to this Cabal file"
  , Opt.Option ['r'] ["root"]
    (Opt.ReqArg (SetRoot . T.pack) "DIRECTORY")
    "Set the root directory for this project"

  , Opt.Option ['c'] ["category"]
    (Opt.ReqArg (SetCategory . T.pack) "CATEGORY")
    "Set the category for this project"
  , Opt.Option ['s'] ["synopsis"]
    (Opt.ReqArg (SetSynopsis . T.pack) "SYNOPSIS")
    "Set the synopsis for this project"
  , Opt.Option ['d'] ["description"]
    (Opt.ReqArg (SetDescription . T.pack) "DESCRIPTION")
    "Set the description for this project"
  , Opt.Option ['l'] ["license"]
    (Opt.ReqArg (SetLicense . T.pack) "LICENSE")
    "Set the license for this project"

  , Opt.Option ['a'] ["add-dep"]
    (Opt.ReqArg (AddDep . T.pack) "PACKAGE")
    "Add a dependency to this application"
  , Opt.Option ['A'] ["add-usual-deps"]
    (Opt.NoArg AddUsualDeps)
    "Add the typical set of dependencies to this application"
  ]


usageInfo :: String
usageInfo = Opt.usageInfo header options
  where header = "Usage: charter (quick|executable|library) [name]"


process :: [Option] -> C.Project -> Either String C.Project
process opts p = foldl (>>=) (return p) (map go opts)
  where
    go (AddBinary n) proj =
      return $ proj & C.binDetails %~ (C.mkBinary n :)
    go (AddMod m) proj =
      return $ proj & C.libDetails %~ fmap (& C.libMods %~ (m :))
    go (SetCategory s) proj =
      return $ proj & C.projectDetails . C.projectCategory .~ Just s
    go (SetSynopsis s) proj =
      return $ proj & C.projectDetails . C.projectSynopsis .~ Just s
    go (SetDescription s) proj =
      return $ proj & C.projectDetails . C.projectDescription .~ Just s
    go (SetLicense license) proj
      | not (license `elem` C.validLicenses) =
        Left $ concat [ "Unknown license: `"
                      , T.unpack license
                      , "'\n\nValid Cabal licenses include:\n  - "
                      , T.unpack (T.intercalate "\n  - " C.validLicenses)
                      ]
      | otherwise =
        return $ proj & C.projectDetails . C.projectLicense .~ Just license
    go (SetRoot _) proj = return proj

    go (AddDep dep) proj =
      return $ proj & C.binDetails %~ fmap (& C.execDeps %~ (dep :))
                    & C.libDetails %~ fmap (& C.libDeps %~ (dep :))
    go (AddUsualDeps) proj =
      return $ proj & C.binDetails %~ fmap (& C.execDeps %~ (C.usualDeps ++))
                    & C.libDetails %~ fmap (& C.libDeps %~ (C.usualDeps ++))

setupProject :: String -> String -> IO C.Project
setupProject typ name = do
  details <- C.projectDefaults (T.pack name)
  case typ of
    "quick"      -> return (C.quickBin details)
    "executable" -> return (C.projectBin details)
    "library"    -> return (C.library details)
    _            -> Sys.die ("unknown project type: " ++ typ ++ "\n" ++ usageInfo)

main :: IO ()
main = do
  args <- Sys.getArgs
  case Opt.getOpt Opt.Permute options args of
    (os, [typ, name], []) -> do
      proj <- process os <$> setupProject typ name
      case proj of
        Right p -> C.createProject p
        Left err -> Sys.die err
    (_, _, errs) -> do
      mapM_ putStrLn errs
      Sys.die usageInfo
