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

{-| Syntax for locations.

i.e. A concrete representation that's (as conveniently as possible) human-readable and human-writeable.

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

Parsing:

* 'LocationPath' — by 'parseLocationPath'.
* 'LocationURI'  — by 'mkURI'.

Validation: TODO

NOTE Syntactically, we require filepaths to start with either:

* a *filepath prefix character* (see 'validFilePathLiteralPrefixSymbols').
* an *environment variable* (in POSIX syntax or in Windows syntax)
* a *letter drive* (for Windows, see 'validFilePathLiteralPrefixSymbols').)

For example, these *filepath literals* are valid:

* @/etc/skeletor@ — an absolute path (starts with @/@)
* @~/skeletor@ — an absolute path (starts with @~@, as part of @~/@)
* @./skeletor@ — a relative path (starts with @.@, as part of @./@)
* @${XDG_CONFIG_HOME}/skeletor@ — an environment variable (starts with @$@, , as part of @${ ... }@)
* @%APPDATA%/skeletor@ — an environment variable (starts with @%@, as part of @% ... %@)
* @C:\\\\skeletor@ — (starts with an uppercase letter, as part of @... :\\\\@)

But these strings are invalid:

* @skeletor@ 
* @etc/skeletor@ 

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

{-| 'LocationFile' identifies (or is) a /string/.

-}

data LocationFile

  = LocationFileInline   Text          -- ^ Write this string.
  | LocationFileFilePath FilePath      -- ^ Copy this file.

  | LocationFileURI URI                -- ^ Download this URI.

  | LocationFileEnvironmentVariable EV -- ^ Read (TODO: a path or URI?) from this environment variable.

  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| A 'LocationDirectory' identifies (or is) a /tree/.

== Notes

For example, in Nixpkgs, by default, @unpackPhase@ can decompress these compressed/archived file-types:

* gzip — @.tar.gz@, @.tgz@, @.tar.Z@.
* bzip2 — @.tar.bz2@, @.tbz2@, @.tbz@.
* xz — @.tar.xz@, @.tar.lzma@, @.txz@.

-}

data LocationDirectory

  = LocationDirectoryInline FileTree        -- ^ Write these strings.
  | LocationDirectoryPath   FilePath        -- ^ Copy this directory, recursively.

  | LocationDirectoryURI URI                -- ^ Download this URL.
  | LocationDirectoryGit URI                -- ^ Clone this Git repository.

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

    LocationParseError msg -> "{{{ LocationParseError.LocationParseError }}} " <>
      msg

--------------------------------------------------

instance IsString LocationParseError where

  fromString = LocationParseError

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-|

-}

parseLocation :: (MonadThrow m) => String -> m Location
parseLocation s = _
  where

    isValid = File.isValid s

    hasDrive

--------------------------------------------------

{-|

-}

validFilePathLiteralPrefixSymbols :: [Char]
validFilePathLiteralPrefixSymbols =

  [ '/'
  , '~'
  , '.'
  , '$'
  , '%'
  ]

--------------------------------------------------

{-|

-}

fromLocation :: Location -> Either LocationParseError URI
fromLocation = _

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------