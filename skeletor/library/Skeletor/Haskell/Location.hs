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

import qualified "unordered-containers" Data.HashMap.Lazy as HashMap
import           "unordered-containers" Data.HashMap.Lazy (HashMap)

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

A 'Location' can be identified by:

* itself — i.e. inline contents literally (into a string or tree).
* a filepath — a directory (the root of the files being located).
* an archive — a `.tar`; to be un-archived (into the above).
* a tarball — a `.tar.gz`; to be decompressed (into the above).
* a URI — download it, following links ([TODO]: detect cycles, ditto with hardlinks of filepaths).
* a Git repository — clone it.
* a name — a known name (e.g. builtin into your system).

-}

data Location

  = LocationInlineFile  Text         -- ^ Write this string.
  | LocationFile        FilePath     -- ^ Copy this file.

  | LocationInlineDirectory FileTree -- ^ Write these strings.
  | LocationDirectory       FilePath -- ^ Copy this directory, recursively.

  | LocationURL         URI          -- ^ Download this URL.
  | LocationGit         URI          -- ^ Clone this Git repository.

  | LocationArchive     FilePath     -- ^ Un-Archive this file, then copy the 'LocationDirectory'.
  | LocationTarball     FilePath     -- ^ Un-Compress this file, un-archive it, then copy the 'LocationDirectory'.

  | LocationEnvironment EV           -- ^ Read (TODO: a path or URI?) from this environment variable.

  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData)

--------------------------------------------------

{-TODO

instance Hashable Location where

  hashWithSalt :: Int -> Location -> Int
  hashWithSalt salt = \case

    LocationInline      x -> c1 `hashWithSalt` salt `hashWithSalt` x
    LocationFile        x -> c2 `hashWithSalt` salt `hashWithSalt` x
    LocationEnvironment x -> c3 `hashWithSalt` salt `hashWithSalt` x
    LocationURL         x -> c4 `hashWithSalt` salt `hashWithSalt` (show x) --TODO

    where

    c0 :: Int
    c0 = 0b0000000000000000000000000000000000000000000000000000000000000000

    c1 :: Int
    c1 = 0b0101010101010101010101010101010101010101010101010101010101010101

    c2 :: Int
    c2 = 0b0011001100110011001100110011001100110011001100110011001100110011

    c3 :: Int
    c3 = 0b0000111100001111000011110000111100001111000011110000111100001111

    c4 :: Int
    c4 = 0b0000000011111111000000001111111100000000111111110000000011111111

    c5 :: Int
    c5 = 0b0000000000000000111111111111111100000000000000001111111111111111

    c6 :: Int
    c6 = 0b0000000000000000000000000000000011111111111111111111111111111111

    -- c7 :: Int
    -- c7 = 0b

    -- c8 :: Int
    -- c8 = 0b

    -- c9 :: Int
    -- c9 = 0b

    -- c10 :: Int
    -- c10 = 0b
-}

--------------------------------------------------
--------------------------------------------------

{-| An in-memory directory (with all its children files).

-}

type FileTree = (HashMap FilePath (Maybe Text)) -- TODO value should be (Maybe Text) to represent an empty directory?

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code







-}
--------------------------------------------------