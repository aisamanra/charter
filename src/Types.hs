{-# LANGUAGE TemplateHaskell #-}

module Types where

import qualified Data.Text as T
import qualified Lens.Family.TH as Lens

data ProjectDetails = ProjectDetails
  { _projectName        :: T.Text
  , _projectAuthor      :: T.Text
  , _projectEmail       :: T.Text
  , _projectYear        :: T.Text
  , _projectCategory    :: Maybe T.Text
  , _projectSynopsis    :: Maybe T.Text
  , _projectDescription :: Maybe T.Text
  , _projectLicense     :: Maybe T.Text
  }

data LibraryDetails = LibraryDetails
  { _libMods :: [T.Text]
  , _libDeps :: [T.Text]
  }

data ExecutableDetails = ExecutableDetails
  { _execName :: T.Text
  , _execDir  :: T.Text
  , _execDeps :: [T.Text]
  }

data Project = Project
  { _projectDetails :: ProjectDetails
  , _libDetails     :: Maybe LibraryDetails
  , _binDetails     :: [ExecutableDetails]
  , _projectRoot    :: Maybe T.Text
  }

Lens.makeLenses ''ProjectDetails
Lens.makeLenses ''LibraryDetails
Lens.makeLenses ''ExecutableDetails
Lens.makeLenses ''Project
