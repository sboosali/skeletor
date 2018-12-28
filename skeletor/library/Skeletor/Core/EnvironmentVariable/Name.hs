--------------------------------------------------
--------------------------------------------------

{-| Environment Variable names.

-}

module Skeletor.Core.EnvironmentVariable.Name where

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

{-| A (cross-platform) environment-variable name.

An environment variable (its name):

* /cannot/ be empty (@""@);
* /must/ TODO;
* /should/ TODO;

See:

* @`toEnvironmentName`   :: Text -> Maybe EnvironmentName@
* @`fromEnvironmentName` :: EnvironmentName -> Text@

-}

newtype EnvironmentName = EnvironmentName

  Text

  deriving stock    (Generic)
  deriving stock    (Show,Read)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | NOTE Calls 'mkEnvironmentName' (a partial function).

instance IsString EnvironmentName where

  fromString = T.pack > mkEnvironmentName

--------------------------------------------------
--------------------------------------------------

{-| Unwrap an 'EnvironmentName' trivially.

-}

fromEnvironmentName :: EnvironmentName -> Text
fromEnvironmentName (EnvironmentName name) = name

--------------------------------------------------

{-| Wrap trivially (no checks).

NOTE Exposing the constructor risks invalid @EnvironmentName@s.

-}

unsafeEnvironmentName :: Text -> EnvironmentName
unsafeEnvironmentName = EnvironmentName

--------------------------------------------------
--------------------------------------------------

{-| 

-}

toEnvironmentName :: (MonadThrow m) => Text -> m EnvironmentName
toEnvironmentName = EnvironmentName > return

--------------------------------------------------

{-| 

NOTE 'toEnvironmentName_byDroppingCharacters' may drop invalid characters.

NOTE 'toEnvironmentName_byDroppingCharacters' is a partial function. It crashes on empty strings.

-}

toEnvironmentName_byDroppingCharacters :: (MonadThrow m) => Text -> m EnvironmentName
toEnvironmentName_byDroppingCharacters = EnvironmentName > return

--------------------------------------------------

{-| 

NOTE 'toEnvironmentName_byChangingCharacters' may replace unconventional characters.

NOTE 'toEnvironmentName_byChangingCharacters' is a partial function. It crashes on empty strings.

-}

toEnvironmentName_byChangingCharacters :: (MonadThrow m) => Text -> m EnvironmentName
toEnvironmentName_byChangingCharacters = EnvironmentName > return

--------------------------------------------------
--------------------------------------------------

{-| 

NOTE 'mkEnvironmentName' is a partial function. It crashes on:

* empty strings;
* strings with invalid characters;
* [TODO- optionally] strings with "unconventional" (\/ "unidiomatic") characters;

-}

mkEnvironmentName :: Text -> EnvironmentName
mkEnvironmentName text = go text
  where

  go = toEnvironmentName > maybe (error msg) id

  msg = "[EnvironmentVariable.Name.mkEnvironmentName] The following string is not a valid name for a cross-platform environment-variable:\n\n    « " <> show text <> " »\n\n" -- TODO Formatting

--------------------------------------------------
--------------------------------------------------

{-| 

-}

isEnvironmentName :: Text -> Either EnvironmentNameError EnvironmentName
isEnvironmentName = EnvironmentName > Right

--------------------------------------------------
--------------------------------------------------