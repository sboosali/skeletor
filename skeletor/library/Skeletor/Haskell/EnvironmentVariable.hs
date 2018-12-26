--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Haskell.EnvironmentVariable where

--------------------------------------------------
--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import qualified "filepath"   System.FilePath as File

--------------------------------------------------

import qualified "text"       Data.Text    as T
--import qualified "text"       Data.Text.IO as T

--------------------------------------------------

import qualified "bytestring" Data.ByteString as B

--import qualified "bytestring" Data.ByteString.Lazy as B

--------------------------------------------------
--------------------------------------------------

import "base" Control.Exception (Exception(..))

--import qualified "base" System.IO as IO

--------------------------------------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------

import "base" Prelude (error)

--------------------------------------------------
--------------------------------------------------

{-| A (cross-platform) environment-variable name.

See:

* @`toEV`   :: Text -> Maybe EV@
* @`fromEV` :: EV -> Text@

-}

newtype EV = EV

  Text

  deriving stock    (Generic)
  deriving stock    (Show,Read)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | NOTE Calls 'mkEV' (a partial function).

instance IsString EV where

  fromString = T.pack > mkEV

--------------------------------------------------
--------------------------------------------------

data EVNameError

  = EVNameErrorCall                 String

  | EVCantBeAnonymous
  | EVCantBeTooLong                 Natural
  | EVMustBeASCII                   Text
  | EVShouldFollowNamingConventions Text

  deriving stock    (Generic,Show,Read,Eq,Ord)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

instance Exception EVNameError where

  displayException = \case

    EVNameErrorCall                 msg -> "«EVNameError.EVNameErrorCall» " <>
      msg

    EVCantBeAnonymous                   -> "«EVNameError.EVCantBeAnonymous» " <>
      "Environment variables can't be the empty string."

    EVCantBeTooLong                 len -> "«EVNameError.EVCantBeTooLong» " <>
      "Environment variables must be between 1 and 255(TODO) characters."

    EVMustBeASCII                   cs  -> "«EVNameError.EVMustBeASCII» " <>
      "Environment variables should contain only ASCII characters."

    EVShouldFollowNamingConventions cs  -> "«EVNameError.EVShouldFollowNamingConventions» " <>
      "Environment variables should follow a naming convention within the intersection of POSIX's and WIN32's, i.e. TODO."

--------------------------------------------------

-- | @≡ 'EVNameErrorCall'@

instance IsString EVNameError where

  fromString = EVNameErrorCall

--------------------------------------------------
--------------------------------------------------

-- data EVConversionStrategy

--   = EVDropInvalidCharacters
--   | EVReplaceInvalidCharacters
--   | EVNormalizeSeparators

--   deriving stock    (Enum,Bounded,Ix)
--   deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
--   deriving anyclass (GEnum)
--   deriving anyclass (NFData,Hashable)

--------------------------------------------------

--------------------------------------------------
--------------------------------------------------

{-| Unwrap trivially.

-}

fromEV :: EV -> Text
fromEV (EV name) = name

--------------------------------------------------

{-| Wrap trivially (no checks).

NOTE Exposing the constructor risks invalid @EV@s.

-}

unsafeToEV :: Text -> EV
unsafeToEV = EV

--------------------------------------------------
--------------------------------------------------

{-| 

-}

toEV :: (MonadThrow m) => Text -> m EV
toEV = EV > return

--------------------------------------------------

{-| 

NOTE 'toEV_byDroppingCharacters' may drop invalid characters.

NOTE 'toEV_byDroppingCharacters' is a partial function. It crashes on empty strings.

-}

toEV_byDroppingCharacters :: (MonadThrow m) => Text -> m EV
toEV_byDroppingCharacters = EV > return

--------------------------------------------------

{-| 

NOTE 'toEV_byChangingCharacters' may replace unconventional characters.

NOTE 'toEV_byChangingCharacters' is a partial function. It crashes on empty strings.

-}

toEV_byChangingCharacters :: (MonadThrow m) => Text -> m EV
toEV_byChangingCharacters = EV > return

--------------------------------------------------
--------------------------------------------------

{-| 

NOTE 'mkEV' is a partial function. It crashes on:

* empty strings;
* strings with invalid characters;
* strings with "unconventional" (\/ "unidiomatic") characters;

-}

mkEV :: Text -> EV
mkEV text = go text
  where

  go = toEV > maybe (error msg) id

  msg = "[EnvironmentVariable.EV] The following string is not a valid name for a cross-platform environment-variable:\n\n    « " <> show text <> " »\n\n" -- TODO Formatting

--------------------------------------------------
--------------------------------------------------

{-| 

-}

isEV :: Text -> Either EVNameError EV
isEV = EV > Right

--------------------------------------------------
--------------------------------------------------
