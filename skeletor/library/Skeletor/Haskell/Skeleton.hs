
--------------------------------------------------
--------------------------------------------------

{-| 

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

-- | @= 'DefaultSkeleton'@

defaultBuiltinHaskellSkeletonName :: BuiltinHaskellSkeletonName
defaultBuiltinHaskellSkeletonName = DefaultSkeleton

--------------------------------------------------

allBuiltinHaskellSkeletonNames :: [BuiltinHaskellSkeletonName]
allBuiltinHaskellSkeletonNames = genum

--------------------------------------------------

{-|

-}

printBuiltinSkeletonName :: BuiltinHaskellSkeletonName -> String
printBuiltinSkeletonName = \case

  DefaultSkeleton -> "default"
  MaximalSkeleton -> "maximal"
  MinimalSkeleton -> "minimal"
  SimpleSkeleton  -> "simple"
  ForeignSkeleton -> "foreign"

--------------------------------------------------

{-| Smart Constructor for names of custom haskell skeleton-skeletons.

Syntactically, a valid 'CustomHaskellSkeletonName':

* has only are alphanumeric characters and/or the hyphen character.
* differs from any 'BuiltinHaskellSkeletonName' (those are “reserved”).

-}

parseCustomSkeletonName :: (MonadThrow m) => String -> m CustomHaskellSkeletonName
parseCustomSkeletonName s =

  if   isCustomSkeletonNameValid s
  then return (CustomHaskellSkeletonName s)
  else throwM exception

  where

  exception = SkeletorHaskellSyntaxError message
  message   = "[parseCustomSkeletonName] " <> (show s) <> " is not a valid 'CustomHaskellSkeletonName'"

--------------------------------------------------

{-| Predicate for valid names (of custom haskell skeleton-skeletons).

Syntactically, a valid 'CustomHaskellSkeletonName':

* has only are alphanumeric characters and/or the hyphen character.
* differs from any 'BuiltinHaskellSkeletonName' (those are “reserved”).

-}

isCustomSkeletonNameValid :: String -> Bool
isCustomSkeletonNameValid s = 

 (not . isReserved) s && areAllCharactersAlphanumericOrHyphen s

  where

  isReserved = (`elem` allPrintedBuiltinHaskellSkeletonNames)

  areAllCharactersAlphanumericOrHyphen = all isCharacterAlphanumericOrHyphen

  isCharacterAlphanumericOrHyphen c = Char.isAlphaNum c || ('-' == c)

  allPrintedBuiltinHaskellSkeletonNames = (printBuiltinSkeletonName <$> allBuiltinHaskellSkeletonNames)

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------