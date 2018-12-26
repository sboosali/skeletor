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

-- | NOTE Calls 'toEV_byErroring' (a partial function).

instance IsString EV where

  fromString = T.pack > toEV_byErroring

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
    EVNameErrorCall                 msg -> msg
    EVCantBeAnonymous                   -> ""
    EVCantBeTooLong                 len -> ""
    EVMustBeASCII                   cs  -> ""
    EVShouldFollowNamingConventions cs  -> ""

--------------------------------------------------

-- | @≡ 'EVNameErrorCall'@

instance IsString EVNameError where

  fromString = EVNameErrorCall

--------------------------------------------------
--------------------------------------------------

{-| Unwrap trivially.

-}

fromEV :: EV -> Text
fromEV (EV name) = name

--------------------------------------------------
--------------------------------------------------

{-| 

-}

toEV :: Text -> Maybe EV
toEV = EV > Just

--------------------------------------------------

{-| 

NOTE 'toEV_byDroppingCharacters' may drop invalid characters.

NOTE 'toEV_byDroppingCharacters' is a partial function. It crashes on empty strings.

-}

toEV_byDroppingCharacters :: Text -> EV
toEV_byDroppingCharacters = EV

--------------------------------------------------

{-| 

NOTE 'toEV_byChangingCharacters' may replace unconventional characters.

NOTE 'toEV_byChangingCharacters' is a partial function. It crashes on empty strings.

-}

toEV_byChangingCharacters :: Text -> EV
toEV_byChangingCharacters = EV

--------------------------------------------------

{-| 

NOTE 'toEV_byErroring' is a partial function. It crashes on:

* empty strings;
* strings with invalid characters;
* strings with "unconventional" (\/ "unidiomatic") characters;

-}

toEV_byErroring :: Text -> EV
toEV_byErroring text = go text
  where

  go = toEV > maybe (error msg) id

  msg = "[EnvironmentVariable.EV] The following string is not a valid name for a cross-platform environment-variable:\n\n    « " <> show text <> " »\n\n" -- TODO Formatting

--------------------------------------------------

{-| Wrap trivially (no checks).

NOTE Exposing the constructor risks invalid @EV@s.

-}

unsafeEV :: Text -> EV
unsafeEV = EV

--------------------------------------------------
--------------------------------------------------

{-| 

-}

isEV :: Text -> Either EVNameError EV
isEV = EV > Right

--------------------------------------------------
--------------------------------------------------
