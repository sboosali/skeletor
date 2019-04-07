--------------------------------------------------
--------------------------------------------------

{-|



-}

module Skeletor.Haskell.Find.Types where

--------------------------------------------------

import Skeletor.Haskell.Chiaroscuro.Types

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "filemanip" System.FilePath.GlobPattern (GlobPattern)

--------------------------------------------------
--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| a 'FileFilter' holds filters:

* for either files or directories or both.
* that will be either blacklisted or whitelisted
(by 'ChiaroscuroFilters').

-}

type FileFilters = ChiaroscuroFilters FilePattern

--------------------------------------------------

{-| a 'FileFilter' holds filters:

* for either files or directories or both.
* that will be either blacklisted or whitelisted
(by 'ChiaroscuroFilters').

-}

type FileFilter = ChiaroscuroFilter FilePattern

--------------------------------------------------
--------------------------------------------------

{-| a 'FilePattern' is a filter (a 'GlobPattern') for either:

* a (regular) file; or
* a directory;

= GlobPattern Syntax

NOTE 'GlobPattern's are not regular expressions; they have only two operators: TODO no there's more

* the single-asterisk, representing one-or-more of any-character.
* the double-asterisk, representing one-or-more (sub)directories. 

From @filemanip@'s documentation:

Basic glob pattern syntax is the same as for the Unix shell
environment.

* @*@ matches everything up to a directory separator or end of
string.

* @[/range/]@ matches any character in /range/.

* @[!/range/]@ matches any character /not/ in /range/.

There are three extensions to the traditional glob syntax, taken
from modern Unix shells.

* @\\@ escapes a character that might otherwise have special
meaning.  For a literal @\"\\\"@ character, use @\"\\\\\"@.

* @**@ matches everything, including a directory separator.

* @(/s1/|/s2/|/.../)@ matches any of the strings /s1/, /s2/, etc.

-}

data FilePattern

  = RegularPattern   GlobPattern
  | DirectoryPattern GlobPattern

  deriving stock    (Generic,Lift)
  deriving stock    (Show,Read,Eq,Ord)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| a (simplified) 'File' is a path ('FilePattern') to either:

* a (regular) file; or
* a directory;

-}

data File

  = RegularFile   FilePath
  | DirectoryFile FilePath

  deriving stock    (Generic,Lift)
  deriving stock    (Show,Read,Eq,Ord)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| a 'FileKind' is a (subset) of the kinds of files.

-}

data FileKind

  = REGULAR
  | DIRECTORY

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)

  deriving stock    (Generic,Lift)
  deriving stock    (Show,Read,Eq,Ord)

  deriving anyclass (NFData,Hashable)

--------------------------------------------------
-- Values ----------------------------------------
--------------------------------------------------

{-| a blacklisted (regular) file pattern.

-}

blacklistedFile :: GlobPattern -> FileFilter
blacklistedFile = blacklistedPatternOf REGULAR

--------------------------------------------------

{-| a whitelisted (regular) file pattern.

-}

whitelistedFile :: GlobPattern -> FileFilter
whitelistedFile = whitelistedPatternOf REGULAR

--------------------------------------------------

{-| a blacklisted directory pattern.

-}

blacklistedDirectory :: GlobPattern -> FileFilter
blacklistedDirectory = blacklistedPatternOf DIRECTORY

--------------------------------------------------

{-| a whitelisted directory pattern.

-}

whitelistedDirectory :: GlobPattern -> FileFilter
whitelistedDirectory = whitelistedPatternOf DIRECTORY

--------------------------------------------------
--------------------------------------------------

{-| a blacklisted pattern.

-}

blacklistedPatternOf :: FileKind -> GlobPattern -> FileFilter
blacklistedPatternOf k = fromFileKind k > Blacklisted

--------------------------------------------------

{-| a whitelisted pattern.

-}

whitelistedPatternOf :: FileKind -> GlobPattern -> FileFilter
whitelistedPatternOf k = fromFileKind k > Whitelisted

--------------------------------------------------

{-| (utility function)

-}

fromFileKind
  :: FileKind
  -> (GlobPattern -> FilePattern)

fromFileKind = \case

  REGULAR   -> RegularPattern   
  DIRECTORY -> DirectoryPattern 

--------------------------------------------------
--------------------------------------------------