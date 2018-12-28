--------------------------------------------------
--------------------------------------------------

{-| Environment Variable values.

-}

module Skeletor.Core.EnvironmentVariable.Value where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Core.EnvironmentVariable.Errors

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

{-| 

-}

toEnvironmentValue :: (MonadThrow m) => Text -> m EnvironmentValue

toEnvironmentValue = EnvironmentValue > return

--------------------------------------------------
--------------------------------------------------

{-| Unwrap an 'EnvironmentValue' trivially.

-}

fromEnvironmentValue :: EnvironmentValue -> Text

fromEnvironmentValue (EnvironmentValue t) = t

--------------------------------------------------

{-| Wrap trivially (no checks).

NOTE Exposing the constructor risks invalid @EnvironmentValue@s.

-}

unsafeEnvironmentValue :: Text -> EnvironmentValue
unsafeEnvironmentValue = EnvironmentValue

--------------------------------------------------
--------------------------------------------------

{-| 

NOTE 'mkEnvironmentValue' is a partial function. It crashes on:

* empty strings;
* strings with invalid characters;
* [TODO- optionally] strings with "unconventional" (\/ "unidiomatic") characters;

-}

mkEnvironmentValue :: Text -> EnvironmentValue
mkEnvironmentValue text = go text
  where

  go = toEnvironmentValue > maybe (error msg) id

  msg = "[EnvironmentVariable.Value.mkEnvironmentValue] The following string is not a valid value for a cross-platform environment-variable:\n\n    « " <> show text <> " »\n\n" -- TODO Formatting

--------------------------------------------------
--------------------------------------------------