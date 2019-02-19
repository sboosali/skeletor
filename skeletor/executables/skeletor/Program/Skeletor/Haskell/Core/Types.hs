{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DataKinds             #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Core.Types where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

--import Skeletor.Core.Types

import Skeletor.Haskell.Types

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "generic-lens" Data.Generics.Product as G

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

--------------------------------------------------
--------------------------------------------------

import Prelude_exe

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

type URL = String

--------------------------------------------------
--------------------------------------------------

{-|

-}

data Status

  = Success
  | Failure

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

toStatus :: (G.HasField' "status" a Status) => a -> Status
toStatus = G.getField @"status"

--------------------------------------------------

isSuccessful :: Status -> Bool
isSuccessful = \case

  Success -> True
  Failure -> False

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

--------------------------------------------------
-------------------------------------------------}