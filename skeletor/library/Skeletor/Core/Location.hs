
--------------------------------------------------
--------------------------------------------------

{-| 'Location's.



-}

module Skeletor.Core.Location

  ( module Skeletor.Core.Location.Types
  , module Skeletor.Core.Location
  ) where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Core.Location.Types

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import           "modern-uri" Text.URI (URI)
import qualified "modern-uri" Text.URI as URI

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "filepath" System.FilePath as File

--------------------------------------------------
--------------------------------------------------

import qualified "base" System.IO as IO

--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-|

-}

parseLocation = Location -> Either LocationParseError URI
parseLocation = _

--------------------------------------------------
--------------------------------------------------