
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

NOTE 'GlobPattern's are not regular expressions; they have only two operators:

* the single-asterisk, representing one-or-more of any-character.
* the double-asterisk, representing one-or-more (sub)directories. 

-}

data FilePattern

  = RegularPattern   GlobPattern
  | DirectoryPattern GlobPattern

  deriving stock    (Generic,Lift)
  deriving stock    (Show,Read,Eq,Ord)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| a 'FileKind' is a (subset) of the kinds of files.

-}

data FileKind

  = RegularFile
  | DirectoryFile

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
blacklistedFile = blacklistedPatternOf RegularFile

--------------------------------------------------

{-| a whitelisted (regular) file pattern.

-}

whitelistedFile :: GlobPattern -> FileFilter
whitelistedFile = whitelistedPatternOf RegularFile

--------------------------------------------------

{-| a blacklisted directory pattern.

-}

blacklistedDirectory :: GlobPattern -> FileFilter
blacklistedDirectory = blacklistedPatternOf DirectoryFile

--------------------------------------------------

{-| a whitelisted directory pattern.

-}

whitelistedDirectory :: GlobPattern -> FileFilter
whitelistedDirectory = whitelistedPatternOf DirectoryFile

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

  RegularFile   -> RegularPattern   
  DirectoryFile -> DirectoryPattern 

--------------------------------------------------
--------------------------------------------------