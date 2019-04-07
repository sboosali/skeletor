
--------------------------------------------------
--------------------------------------------------

{-| 'Exception' Types.

-}

module Skeletor.Core.Errors

  ( LocationParseError(..)

  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------

import "base" Control.Exception (Exception(..))

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-|

-}

data LocationParseError

  = LocationParseError String

  deriving stock    (Generic,Lift)
  deriving stock    (Show,Read,Eq,Ord)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

instance Exception LocationParseError where

  displayException :: LocationParseError -> String
--displayException = show
  displayException = \case

    LocationParseError msg -> "{{{ LocationParseError.LocationParseError }}} " <>
      msg

--------------------------------------------------

instance IsString LocationParseError where

  fromString = LocationParseError

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------