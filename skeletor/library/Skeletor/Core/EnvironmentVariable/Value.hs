--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.EnvironmentVariable.Value where

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

{-| A (cross-platform) environment-variable value.

The value of an environment variable:

* /cannot/ be empty (@""@);
* /cannot/ have any equal signs (@'='@);

See:

* @`toEnvironmentValue`   :: Text -> Maybe EnvironmentValue@
* @`fromEnvironmentValue` :: EnvironmentValue -> Text@

-}

newtype EnvironmentValue = EnvironmentValue

  Text

  deriving stock    (Generic)
  deriving stock    (Show,Read)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | NOTE Calls 'mkEnvironmentValue' (a partial function).

instance IsString EnvironmentValue where

  fromString = T.pack > mkEnvironmentValue

--------------------------------------------------
--------------------------------------------------

toEnvironmentValue :: Text -> Maybe EnvironmentValue

toEnvironmentValue t = _

--------------------------------------------------
--------------------------------------------------

fromEnvironmentValue :: EnvironmentValue -> Text

fromEnvironmentValue (EnvironmentValue t) = t

--------------------------------------------------
--------------------------------------------------