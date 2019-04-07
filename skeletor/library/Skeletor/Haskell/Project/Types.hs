--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Haskell.Project.Types where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------

import Skeletor.Haskell.Types
import Skeletor.Haskell.Errors

--------------------------------------------------
--------------------------------------------------

import qualified "base" Data.Char as Char

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-|

-}

-- :: _ -> _
-- = _

--------------------------------------------------
--------------------------------------------------

{-| 

-}

type ProjectName = UnknownOr BuiltinHaskellProjectName

--------------------------------------------------
--------------------------------------------------

{-| 

-}

type HaskellProjectName = Either CustomHaskellProjectName BuiltinHaskellProjectName

--type HaskellProjectIdentifier

--------------------------------------------------
--------------------------------------------------

{-|

-}

newtype CustomHaskellProjectName = CustomHaskellProjectName

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| 

-}

data BuiltinHaskellProjectName

  = DefaultProject
  | MaximalProject
  | MinimalProject
  | SimpleProject
  | ForeignProject

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultKnownProject'@

instance Default BuiltinHaskellProjectName where
  def = defaultBuiltinHaskellProjectName

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

-- | @= 'DefaultProject'@

defaultBuiltinHaskellProjectName :: BuiltinHaskellProjectName
defaultBuiltinHaskellProjectName = DefaultProject

--------------------------------------------------

allBuiltinHaskellProjectNames :: [BuiltinHaskellProjectName]
allBuiltinHaskellProjectNames = genum

--------------------------------------------------

{-|

-}

printBuiltinProjectName :: BuiltinHaskellProjectName -> String
printBuiltinProjectName = \case

  DefaultProject -> "default"
  MaximalProject -> "maximal"
  MinimalProject -> "minimal"
  SimpleProject  -> "simple"
  ForeignProject -> "foreign"

--------------------------------------------------

{-| Smart Constructor for names of custom haskell project-skeletons.

Syntactically, a valid 'CustomHaskellProjectName':

* has only are alphanumeric characters and/or the hyphen character.
* differs from any 'BuiltinHaskellProjectName' (those are “reserved”).

-}

parseCustomProjectName :: (MonadThrow m) => String -> m CustomHaskellProjectName
parseCustomProjectName s =

  if   isCustomProjectNameValid s
  then return (CustomHaskellProjectName s)
  else throwM exception

  where

  exception = SkeletorHaskellSyntaxError message
  message   = "[parseCustomProjectName] " <> (show s) <> " is not a valid 'CustomHaskellProjectName'"

--------------------------------------------------

{-| Predicate for valid names (of custom haskell project-skeletons).

Syntactically, a valid 'CustomHaskellProjectName':

* has only are alphanumeric characters and/or the hyphen character.
* differs from any 'BuiltinHaskellProjectName' (those are “reserved”).

-}

isCustomProjectNameValid :: String -> Bool
isCustomProjectNameValid s = 

 (not . isReserved) s && areAllCharactersAlphanumericOrHyphen s

  where

  isReserved = (`elem` allPrintedBuiltinHaskellProjectNames)

  areAllCharactersAlphanumericOrHyphen = all isCharacterAlphanumericOrHyphen

  isCharacterAlphanumericOrHyphen c = Char.isAlphaNum c || ('-' == c)

  allPrintedBuiltinHaskellProjectNames = (printBuiltinProjectName <$> allBuiltinHaskellProjectNames)

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------