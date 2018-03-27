{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Charter
( module Types
, projectBin
, quickBin
, library

, validLicenses

, mkBinary
, projectDefaults
, createProject
, usualDeps
) where

import           Control.Monad (forM_)
import qualified Data.Char as Char
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Lens.Family
import qualified System.Directory as Sys
import qualified System.Environment as Sys
import qualified System.FilePath as Sys
import qualified System.Process as Proc

import Types
import Templates


mkBinary :: T.Text -> ExecutableDetails
mkBinary n = ExecutableDetails
  { _execName = n
  , _execDir  = n
  , _execDeps = []
  }

mkLibrary :: LibraryDetails
mkLibrary = LibraryDetails
  { _libMods = []
  , _libDeps = []
  }

mkProject :: ProjectDetails -> Project
mkProject deets = Project
  { _projectDetails = deets
  , _libDetails = Nothing
  , _binDetails = []
  , _projectRoot = Nothing
  }

quickBin :: ProjectDetails -> Project
quickBin deets =
  mkProject deets & binDetails .~ [ bin ]
  where bin = mkBinary (deets^.projectName)
                & execDir .~ "src"

projectBin :: ProjectDetails -> Project
projectBin deets =
  mkProject deets & binDetails .~ [ bin ]
                  & libDetails .~ Just lib
  where bin = mkBinary name & execDeps .~ [name]
        lib = mkLibrary & libMods .~ [capitalize name]
        name = deets^.projectName

usualDeps :: [T.Text]
usualDeps =
  [ "text"
  , "containers"
  , "unordered-containers"
  , "bytestring"
  ]

library :: ProjectDetails -> Project
library deets =
  mkProject deets & libDetails .~ Just mkLibrary

mkdirBase :: T.Text -> [T.Text] -> IO ()
mkdirBase base fp = do
  let path = T.unpack (T.intercalate "/" (base:fp))
  Sys.createDirectoryIfMissing True path

writeBase :: T.Text -> [T.Text] -> T.Text -> IO ()
writeBase base fp contents = do
  mkdirBase base (init fp)
  let path = T.unpack (T.intercalate "/" (base:fp))
  putStrLn ("- creating file `" <> path <> "'")
  T.writeFile path contents

run :: String -> [String] -> IO T.Text
run x xs = (T.strip . T.pack) `fmap` Proc.readProcess x xs ""

projectDefaults :: T.Text -> IO ProjectDetails
projectDefaults _projectName = do
  _projectAuthor <- run "git" ["config", "user.name"]
  _projectEmail <- run "git" ["config", "user.email"]
  _projectYear <- run "date" ["+%Y"]
  let _projectCategory = Nothing
      _projectSynopsis = Nothing
      _projectDescription = Nothing
      _projectLicense = Nothing
  return ProjectDetails { .. }

-- | Capitalize just the first letter of a string
capitalize :: T.Text -> T.Text
capitalize t = case T.uncons t of
  Nothing -> mempty
  Just (x, xs) -> T.cons (Char.toUpper x) xs

-- | Actually build out the scaffolding for a project
createProject :: Project -> IO ()
createProject pr = do
  let deets = pr^.projectDetails
  let write = writeBase (deets^.projectName)

  let cabalFile =
        [ cabalHeader deets ] <>
        maybe [] (pure . cabalLibrary) (pr^.libDetails) <>
        map cabalExecutable (pr^.binDetails)

  T.putStrLn ("Creating project `" <> deets^.projectName <> "'")

  let cabalPath = [deets^.projectName <> ".cabal"]
  write cabalPath (T.unlines cabalFile)

  case (pr^.libDetails) of
    Nothing -> return ()
    Just lib -> do
      forM_ (lib^.libMods) $ \ m -> do
        let modPath = "src" : T.splitOn "." m
            modPath' = init modPath ++ [last modPath <> ".hs"]
        write modPath' (defaultLib m)

  forM_ (pr^.binDetails) $ \e -> do
    write [e^.execDir, "Main.hs"] defaultBin

  let pr = (Proc.proc "git" ["init"])
             { Proc.cwd = Just (T.unpack (deets^.projectName)) }
  _ <- Proc.withCreateProcess pr (\_ _ _ -> Proc.waitForProcess)
  write [".gitignore"] defaultGitignore
  return ()


validLicenses :: [T.Text]
validLicenses =
  [ "GPL", "AGPL", "LGPL",
    "BSD2", "BSD3", "BSD4",
    "MIT", "ISC", "MPL",
    "Apache", "PublicDomain",
    "AllRightsReserved",
    "UnspecifiedLicense",
    "OtherLicense"
  ]
