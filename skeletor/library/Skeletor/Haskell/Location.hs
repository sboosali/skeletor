{-# LANGUAGE BinaryLiterals #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Haskell.Location where

--------------------------------------------------

import Skeletor.Haskell.EnvironmentVariable

--------------------------------------------------
--------------------------------------------------

import           "modern-uri" Text.URI (URI)
import qualified "modern-uri" Text.URI as URI

--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import qualified "filepath"   System.FilePath as File

--------------------------------------------------

import qualified "text"       Data.Text    as T
import qualified "text"       Data.Text.IO as T

--------------------------------------------------

import qualified "bytestring" Data.ByteString as B

--import qualified "bytestring" Data.ByteString.Lazy as B

--------------------------------------------------

--import qualified "base" System.IO as IO

import Prelude_location

--------------------------------------------------
--------------------------------------------------

{-|

-}

--------------------------------------------------
--------------------------------------------------

{-|

-}

data Location

  = LocationInline      Text         -- ^ Write this string.
  | LocationFile        FilePath     -- ^ Copy this file.
  | LocationEnvironment EV           -- ^ Read from this environment variable.
  | LocationURL         URI          -- ^ Download this URL.

  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData)

--------------------------------------------------

instance Hashable Location where

  hashWithSalt :: Int -> Location -> Int
  hashWithSalt salt = \case

    LocationInline      x -> c1 `hashWithSalt` salt `hashWithSalt` x
    LocationFile        x -> c2 `hashWithSalt` salt `hashWithSalt` x
    LocationEnvironment x -> c3 `hashWithSalt` salt `hashWithSalt` x
    LocationURL         x -> c4 `hashWithSalt` salt `hashWithSalt` (show x) --TODO

    where

    c1 :: Int
    c1 = 0b0000000000000000000000000000000000000000000000000000000000000000

    c2 :: Int
    c2 = 0b0101010101010101010101010101010101010101010101010101010101010101

    c3 :: Int
    c3 = 0b0011001100110011001100110011001100110011001100110011001100110011

    c4 :: Int
    c4 = 0b0000111100001111000011110000111100001111000011110000111100001111

    -- c5 :: Int
    -- c5 = 0b0000000000000000000000000000000011111111111111111111111111111111

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code







-}
--------------------------------------------------