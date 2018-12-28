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

-- import Skeletor.Core.

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "filepath" System.FilePath as File

--------------------------------------------------
--------------------------------------------------

import qualified "base" System.IO as IO

import           "base" Control.Exception (Exception(..))

--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
-- Definitions -----------------------------------
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