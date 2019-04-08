
--------------------------------------------------
--------------------------------------------------

{-| 

A few builtin project-skeletons are included, as @data-files@
(TODO or injected into the executeable via @TemplateHaskell?).

-}

module Skeletor.Haskell.Skeleton where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------

import Skeletor.Core.Location

--------------------------------------------------

import Skeletor.Haskell.Types
import Skeletor.Haskell.Errors

--------------------------------------------------
--------------------------------------------------

import qualified "base" Data.Char as Char

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| Identifies a project-skeleton, either:

* by location (a @URI@), or
* by name (a string, without whitespace).

-}

data SkeletonIdentifier

  = SkeletonByName     String
  | SkeletonByLocation Location

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

--------------------------------------------------

-- | @= 'defaultSkeletonIdentifier'@

instance Default SkeletonIdentifier where

  def = defaultSkeletonIdentifier

--------------------------------------------------

-- | @= 'SkeletonByName' "default"@

defaultSkeletonIdentifier :: SkeletonIdentifier
defaultSkeletonIdentifier = SkeletonByName "default"

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-| Predicate for valid names (of custom haskell skeleton-skeletons).

Syntactically, a valid 'CustomHaskellSkeletonName':

* has only printable, non-whitespace characters.
* differs from any 'reservedSkeletonNames'.

-}

isCustomSkeletonNameValid :: String -> Bool
isCustomSkeletonNameValid s

   = areAllCharactersVisible s
  && (not . isReserved) s

  where

  isReserved = (`elem` reservedSkeletonNames)

  areAllCharactersVisible = all isCharacterPrintableAndNotWhitespace

  isCharacterPrintableAndNotWhitespace c = Char.isPrint c && Char.isSpace c

--------------------------------------------------

reservedSkeletonNames :: [String]
reservedSkeletonNames = [ "default" ]

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------