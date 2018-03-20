{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates where

import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Lens.Family

import Types

cabalHeader :: ProjectDetails -> T.Text
cabalHeader pr = T.unlines
  [ "name: " <> pr^.projectName
  , "version: 0.1.0.0"
  , case pr^.projectSynopsis of
      Just s -> "synopsis: " <> s
      Nothing -> "-- synopsis:"
  , case pr^.projectDescription of
      Just s -> "description: " <> s
      Nothing -> "-- description:"
  , case pr^.projectLicense of
      Nothing -> "license: BSD3"
      Just l  -> "license: " <> l
  , "author: " <> pr^.projectAuthor <> " <" <> pr^.projectEmail <> ">"
  , "maintainer: " <> pr^.projectAuthor <> " <" <> pr^.projectEmail <> ">"
  , "copyright: @" <> pr^.projectYear <> " " <> pr^.projectAuthor
  , case pr^.projectCategory of
      Just c  -> "category: " <> c
      Nothing -> "-- category:"
  , "build-type: Simple"
  , "cabal-version: >=1.14"
  ]

cabalLibrary :: LibraryDetails -> T.Text
cabalLibrary lib = T.unlines $
  [ "library"
  , "  hs-source-dirs: src"
  , "  ghc-options: -Wall"
  , "  build-depends: base >=4.7 && <5"
  , "  default-language: Haskell2010"
  , "  default-extensions: ScopedTypeVariables"
  ] <> mods
  where
    mods = case lib^.libExposedModules of
             []     -> []
             (x:xs) ->
               ("  exposed-modules: " <> x) :
               ["                 , " <> m | m <- xs ]

cabalExecutable :: ExecutableDetails -> T.Text
cabalExecutable exe = T.unlines $
  [ "executable " <> exe^.execName
  , "  hs-source-dirs: " <> exe^.execDir
  , "  main-is: Main.hs"
  , "  default-language: Haskell2010"
  , "  default-extensions: ScopedTypeVariables"
  , "  ghc-options: -Wall"
  ] <> deps
  where
    baseDep = "  build-depends: base >=4.7 && <5"
    deps =
      baseDep : [ "               , " <> m
                | m <- exe^.execDeps
                ]

defaultBin :: T.Text
defaultBin = T.unlines $
  [ "module Main where"
  , ""
  , "main :: IO ()"
  , "main = return ()"
  ]

defaultLib :: T.Text -> T.Text
defaultLib mod = T.unlines $
  [ "module " <> mod
  , "("
  , ") where"
  ]

defaultGitignore :: T.Text
defaultGitignore = T.unlines
  [ "dist"
  , "dist-*"
  , "*~"
  , "cabal-dev"
  , "*.o"
  , "*.hi"
  , "*.chi"
  , "*.chs.h"
  , "*.dyn_o"
  , "*.dyn_hi"
  , ".hpc"
  , ".hsenv"
  , ".cabal-sandbox/"
  , "cabal.sandbox.config"
  , "*.prof"
  , "*.aux"
  , "*.hp"
  , "*.eventlog"
  , "cabal.project.local"
  , ".ghc.environment.*"
  ]
