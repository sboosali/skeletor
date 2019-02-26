{-# LANGUAGE DuplicateRecordFields #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Action.Types where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Core.Types

--------------------------------------------------

--import Skeletor.Core.Types
import Skeletor.Haskell.Types

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

--------------------------------------------------
--------------------------------------------------

import Prelude_exe

--------------------------------------------------
--------------------------------------------------

{-|

-}

newtype Actions = Actions

  [Action]

  deriving stock    (Show)
  deriving stock    (Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (Semigroup,Monoid)
  deriving newtype  (NFData)

--------------------------------------------------

-- | 
instance IsList Actions where

  type Item Actions = Action

  fromList = coerce
  toList   = coerce

--------------------------------------------------
--------------------------------------------------

{-| 

-}

data Action

  = ActionPrintVersion
  | ActionPrintLicense

  | ActionCreateProject   CreateProject
  | ActionDownloadProject DownloadProject

  | ActionResolveConfiguration

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

{-
  | Action
  | Action
  | Action
-}

--------------------------------------------------
--------------------------------------------------

{-|

-}

data CreateProject = CreateProject

  { location    :: Location
  , destination :: FilePath
  , license     :: License
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data DownloadProject = DownloadProject

  { location    :: Location
  , destination :: FilePath
  , method      :: FetchBy
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data ProjectCreated = ProjectCreated

  { status :: Status
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data ProjectDownloaded = ProjectDownloaded

  { status :: Status
  , path   :: FilePath
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
{-------------------------------------------------



{-|

-}

data XYZ = XYZ

  { :: 
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)



--------------------------------------------------
-------------------------------------------------}