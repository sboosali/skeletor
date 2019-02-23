{-# LANGUAGE DuplicateRecordFields #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Program.Skeletor.Haskell.Command.Types where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

--import Skeletor.Core.Types
import Skeletor.Haskell.Types

--------------------------------------------------

import Program.Skeletor.Haskell.Options.Types
import Program.Skeletor.Haskell.Config.Types
import Program.Skeletor.Haskell.Core.Types

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_exe

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-| 

-}

data Command

  = CommandCreateProject        CreateProjectOptions
  | CommandDownloadProject      DownloadProjectOptions
  | CommandResolveConfiguration 

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data CreateProjectOptions = CreateProjectOptions

  { globals     :: GlobalOptions
  , location    :: Location
  , destination :: FilePath
  , license     :: License
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data DownloadProjectOptions = DownloadProjectOptions

  { globals     :: GlobalOptions
  , location    :: Location
  , destination :: FilePath
  , method      :: FetchBy
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------
