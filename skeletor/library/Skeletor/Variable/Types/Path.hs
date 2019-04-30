--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs     #-}

--------------------------------------------------

{- | /Path-like/ template\/configuration variables.

Types:

* `Path` — (the primary type).

Functions:

* `toPath` — smart constructor for `Path`s.

Links:

* <https://yakking.branchable.com/posts/falsehoods-programmers-believe-about-file-paths/>
* <https://docs.microsoft.com/en-us/windows/desktop/fileio/naming-a-file>

Related:

* The (deprecated) @system-filepath@.

-}

module Skeletor.Variable.Types.Path

  ( module Skeletor.Variable.Types.Path

  , FilePath

  ) where

--------------------------------------------------
-- Imports (Internal) -----------------------------
--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "lens" Control.Lens as L
import           "lens" Control.Lens ( Prism', Iso' )

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "containers" Data.Map as Map

--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------

import           "base" Data.Ratio
  
--------------------------------------------------

import qualified "base" Prelude

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{- | Paths.

Should be cross-platform (validated via `toPath`).

Should be a /regular file/ or a /directory/.

== Examples

-}

data Path

  = PathBytes PosixPath
  | PathChars Win32Path

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Data,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @`toPath`@
instance IsString Path where

  fromString :: String -> Path
  fromString = toPath

--------------------------------------------------
--------------------------------------------------

{- | @POSXI@ Paths.

Operating-Systems:

* @Linux@
* @OS X@

Should be cross-platform (validated via `toPath`).

Should be a /regular file/ or a /directory/.

== Examples

-}

newtype PosixPath = PosixPath

  (ByteString)

  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @`toPosixPath`@
instance IsString PosixPath where

  fromString :: String -> PosixPath
  fromString = toPosixPath

--------------------------------------------------
--------------------------------------------------

{- | @Win32@ Paths.

Operating-Systems:

* @Windows@

-}

newtype Win32Path = Win32Path

  (CI Text)

  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @`toWin32Path`@
instance IsString Win32Path where

  fromString :: String -> Win32Path
  fromString = toWin32Path

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

{- | Reserved File-Names.

-}

reservedPaths :: [FilePath]
reservedPaths =

  [ "CON"
  , "PRN"
  , "AUX"
  , "NUL"
  , "COM1"
  , "COM2"
  , "COM3"
  , "COM4"
  , "COM5"
  , "COM6"
  , "COM7"
  , "COM8"
  , "COM9"
  , "LPT1"
  , "LPT2"
  , "LPT3"
  , "LPT4"
  , "LPT5"
  , "LPT6"
  , "LPT7"
  , "LPT8"
  , "LPT9"
   ]

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

-- | Parse a `Path`.

toPath :: String -> Path
toPath s

  = boolByName s
  & maybe (PathS s) PathB

--------------------------------------------------

{- | Unpack a `Path`.

Output:

* @Just False@ represents `falsy`.
* @Just True@ represents `truthy`.
* @Nothing@ represents neither\/unknown.

-}

fromPath :: Path -> Maybe Bool
fromPath = \case

  PathB b -> Just b
  PathS s -> boolByName s

--------------------------------------------------

{- | Is it “falsy”.

== /NOTE/

A `Path` may be neither `falsy` nor `truthy`.

-}

falsy :: Path -> Bool
falsy = \case

  PathB b -> b == False
  PathS s -> boolByName s & maybe False (== False) 

--------------------------------------------------

{- | Is it “truthy”.

== /NOTE/

A `Path` may be neither `truthy` nor `falsy`.

-}

truthy :: Path -> Bool
truthy = \case

  PathB b -> b == True
  PathS s -> boolByName s & maybe False (== True) 

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

-- | Lookup `namedBools`.

boolByName :: String -> Maybe Bool
boolByName s = Map.lookup (lowercase s) namedBools
  where

  lowercase = fmap toLower

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

--------------------------------------------------
-- Doctest ---------------------------------------
--------------------------------------------------

{- $setup

>>> :set -XOverloadedStrings

-}

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-------------------------------------------------

-------------------------------------------------}
--------------------------------------------------
-- TODO ------------------------------------------
--------------------------------------------------
{-------------------------------------------------

FilePaths are:

* unencoded on Linux —  a FilePath is any `NUL`-terminated ByteString.
* UTF16-encoded on Windows — a FilePath is `Text`.

modules

`FilePath` re-exports `FilePath.Portable`
`FilePath.Portable` is the intersection of `FilePath.` and `FilePath.` (module the 'pathSeparator').
`FilePath.Linux` is 'FilePath's are 'ByteString's (i.e. unencoded bytes, `NUL`-terminated and validated against "FilePath.Linux.isFilePath").
`FilePath.Windows` is. 'FilePath's are 'Text' (i.e. UTF16-encoded strings, validated against "FilePath.Windows.isFilePath").

-------------------------------------------------}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------