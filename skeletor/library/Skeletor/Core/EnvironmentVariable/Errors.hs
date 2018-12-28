--------------------------------------------------
--------------------------------------------------

{-| Core types.

Errors for Parsing and Validation of Environment Variables' names
and their values.

= Types

* `EnvironmentNameError` — represents invalid names.
* `EnvironmentValueError` — represents invalid values.

-}

module Skeletor.Core.EnvironmentVariable.Errors where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

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

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import qualified "text"       Data.Text    as T

--------------------------------------------------

import "base" Control.Exception (Exception(..))

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import "base" Prelude (error)

--------------------------------------------------

import Prelude_location

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

data EnvironmentNameError

  = EnvironmentNameErrorCall String

  | EnvironmentNameCantBeAnonymous
  | EnvironmentNameCantBeTooLong                 Natural
  | EnvironmentNameMustBeASCII                   Text
  | EnvironmentNameShouldFollowNamingConventions Text

  deriving stock    (Generic,Show,Read,Eq,Ord)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

instance Exception EnvironmentNameError where

  displayException = \case

    EnvironmentNameErrorCall                 msg -> "«EnvironmentNameError.EnvironmentNameErrorCall» " <>
      msg

    EnvironmentNameCantBeAnonymous                   -> "«EnvironmentNameError.EnvironmentNameCantBeAnonymous» " <>
      "Environment variables can't be the empty string."

    EnvironmentNameCantBeTooLong                 len -> "«EnvironmentNameError.EnvironmentNameCantBeTooLong» " <>
      "Environment variables must be between 1 and 255(TODO) characters."

    EnvironmentNameMustBeASCII                   cs  -> "«EnvironmentNameError.EnvironmentNameMustBeASCII» " <>
      "Environment variables should contain only ASCII characters."

    EnvironmentNameShouldFollowNamingConventions cs  -> "«EnvironmentNameError.EnvironmentNameShouldFollowNamingConventions» " <>
      "Environment variables should follow a naming convention within the intersection of POSIX's and WIN32's, i.e. TODO."

--------------------------------------------------

-- | @≡ 'EnvironmentNameErrorCall'@

instance IsString EnvironmentNameError where

  fromString = EnvironmentNameErrorCall

--------------------------------------------------
--------------------------------------------------

data EnvironmentValueError

  = EnvironmentValueErrorCall String

  | EnvironmentValueCantBeEmpty
  | EnvironmentValueCantBeTooLong             Natural
  | EnvironmentValueMustBeASCII               Text
  | EnvironmentValueCantHaveInvalidCharacters [Char]

  deriving stock    (Generic,Show,Read,Eq,Ord)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

instance Exception EnvironmentValueError where

  displayException = \case

    EnvironmentValueErrorCall                 msg -> "«EnvironmentValueError.EnvironmentValueErrorCall» " <>
      msg

    EnvironmentValueCantBeEmpty                   -> "«EnvironmentValueError.EnvironmentValueCantBeEmpty» " <>
      "Environment variables (on Windows) can't be the empty string."

    EnvironmentValueCantBeTooLong                 len -> "«EnvironmentValueError.EnvironmentValueCantBeTooLong» " <>
      "Environment variables must be between 1 and 255(TODO) characters."

    EnvironmentValueMustBeASCII                   cs  -> "«EnvironmentValueError.EnvironmentValueMustBeASCII» " <>
      "Environment variables should contain only ASCII characters."

    EnvironmentValueCantHaveInvalidCharacters cs  -> "«EnvironmentValueError.EnvironmentValueCantHaveInvalidCharacters» " <>
      "Environment variables can't hold the equal-sign (on Windows), i.e. TODO."

--------------------------------------------------

-- | @≡ 'EnvironmentValueErrorCall'@

instance IsString EnvironmentValueError where

  fromString = EnvironmentValueErrorCall

--------------------------------------------------
--------------------------------------------------