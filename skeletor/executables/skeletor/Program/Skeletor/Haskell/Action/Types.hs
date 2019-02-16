{-# LANGUAGE DuplicateRecordFields #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Action.Types where

--------------------------------------------------
-- Imports (Internal) ----------------------------
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

type URL = String

--------------------------------------------------
--------------------------------------------------

{-|

-}

newtype Actions = Actions

  [Action]

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

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

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

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

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data DownloadProject = DownloadProject

  { location    :: Location
  , destination :: FilePath
  , method      :: FetchBy
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

data Location

  = LocationStdin
  | LocationPath FilePath
  | LocationURL  URL

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| How to fetch (a.k.a download) a location.

TODO When fetching @URL@s, 'FetchBy' specifies an HttpTransport@.

-}

data FetchBy

  = FetchByHaskell
  | FetchByCurl
  | FetchByWget

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
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