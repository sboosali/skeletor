--------------------------------------------------
--------------------------------------------------

{-| Core types.

-}

module Skeletor.Core.EnvironmentVariable.Types

  ( module Skeletor.Core.EnvironmentVariable.Types
  , module Skeletor.Core.EnvironmentVariable.Errors
  , module Skeletor.Core.EnvironmentVariable.Name
  , module Skeletor.Core.EnvironmentVariable.Value
  ) where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Core.EnvironmentVariable.Errors
import Skeletor.Core.EnvironmentVariable.Name
import Skeletor.Core.EnvironmentVariable.Value

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------
-- Imports (Standard Library) --------------------
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

data EnvironmentNameConversionStrategy

  = EnvironmentNameDropInvalidCharacters
  | EnvironmentNameReplaceInvalidCharacters
  | EnvironmentNameNormalizeWordSeparators

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (GEnum)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------