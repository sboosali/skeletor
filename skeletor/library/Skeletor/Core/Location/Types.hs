--------------------------------------------------
--------------------------------------------------

{-| a 'Location' can:

* be local or remote;
* represent either a file or a directory-tree;
* reference several encodings
(plaintext, archived, compressed, version-controlled, etc);

-}

module Skeletor.Core.Location.Types where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Core.EnvironmentVariable

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--------------------------------------------------

import           "modern-uri" Text.URI (URI)
import qualified "modern-uri" Text.URI as URI

--------------------------------------------------

import qualified "filepath" System.FilePath as File

--------------------------------------------------
--------------------------------------------------

import qualified "unordered-containers" Data.HashMap.Lazy as Map
import           "unordered-containers" Data.HashMap.Lazy (HashMap)

--------------------------------------------------

import qualified "base" System.IO as IO

--------------------------------------------------

import           "base" Control.Exception (Exception(..))

--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-| Syntax for (i.e. a concrete, human-readable representation of) locations.

Locate by (any of) these syntaxes:

* URI  — e.g. @file://D:\\@, @https://github.com/sboosali/skeletor.git@, @git://git@github.com:sboosali/skeletor.git@
* Path — e.g. @/usr/local/share@, @~/.local/share@, @./.@

Examples URIs:

* @file://D:\\@
* @https://github.com/sboosali/skeletor@
* @git://git@github.com:sboosali/skeletor.git@

Example Paths:

* @./.@
* @~/.local/share@
* @/usr/local/share@

-}

data LocationSyntax

  = LocationURI  URI
  | LocationPath Path

  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-| Locate either a 'LocationFile' or a 'LocationDirectory'.

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

  = LocationFile      LocationFile
  | LocationDirectory LocationDirectory

  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| 'LocationFile' is identifies (or is) a string.

-}

data LocationFile

  = LocationFileInline   Text          -- ^ Write this string.
  | LocationFileFilePath FilePath      -- ^ Copy this file.

  | LocationFileURL URL                -- ^ Download this URL.

  | LocationFileEnvironmentVariable EV -- ^ Read (TODO: a path or URI?) from this environment variable.

  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| A 'LocationDirectory'.

-}

data LocationDirectory

  = LocationDirectoryInline FileTree        -- ^ Write these strings.
  | LocationDirectoryPath   FilePath        -- ^ Copy this directory, recursively.

  | LocationDirectoryURL URL                -- ^ Download this URL.
  | LocationDirectoryGit URL                -- ^ Clone this Git repository.

  | LocationDirectoryArchive FilePath       -- ^ Un-Archive this file, then copy the 'LocationDirectory'.
  | LocationDirectoryTarball FilePath       -- ^ Un-Compress this file, un-archive it, then copy the 'LocationDirectory'.

  | LocationDirectoryEnvironmentVariable EV -- ^ Read (TODO: a path or URI?) from this environment variable.

  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|

-}

newtype FileTree = FileTree

  (HashMap FilePath String)         -- TODO Text

  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)


--------------------------------------------------
--------------------------------------------------

{-|

-}

type URL = FilePath

--------------------------------------------------
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

    LocationParseError msg -> "«LocationParseError.LocationParseError» " <>
      msg

--------------------------------------------------

instance IsString LocationParseError where

  fromString = LocationParseError

--------------------------------------------------
--------------------------------------------------