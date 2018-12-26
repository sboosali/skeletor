
--------------------------------------------------
--------------------------------------------------

{-|



-}

module Skeletor.Haskell.Types where

--------------------------------------------------
--------------------------------------------------

import qualified "unordered-containers" Data.HashMap.Lazy as HashMap
import           "unordered-containers" Data.HashMap.Lazy (HashMap)

--------------------------------------------------
--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
--------------------------------------------------

type UnknownOr = Either String    -- TODO

--------------------------------------------------
--------------------------------------------------

{-| 

-}

type ProjectIdentifier = UnknownOr KnownProject

--------------------------------------------------

{-| 

-}

data KnownProject

  = DefaultHaskellProject
  
  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultKnownProject'@

instance Default KnownProject where
  def = defaultKnownProject

-- | @= 'DefaultHaskellProject'@

defaultKnownProject :: KnownProject
defaultKnownProject = DefaultHaskellProject

--------------------------------------------------
--------------------------------------------------

{-|

-}

newtype Files a = Files

  (HashMap FilePath a)

  deriving stock    (Functor,Foldable,Traversable)
  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData)
  deriving newtype  (Hashable1)

--------------------------------------------------

-- | 
deriving anyclass instance (Hashable a) => Hashable (Files a)

--------------------------------------------------

-- | 
instance (Ord a) => IsList (Files a) where

  type Item (Files a) = (FilePath, a)
  fromList = HashMap.fromList > coerce
  toList   = coerce           > HashMap.toList

--------------------------------------------------

{-|

@
≡ 'Map.empty'
@

-}

emptyFiles :: Files a
emptyFiles = Files HashMap.empty

--------------------------------------------------
--------------------------------------------------

{-|

-}

newtype FileTree = FileTree

  (Map FilePath String)         -- TODO Text

  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData)

instance IsList FileTree where
  type Item FileTree = (FilePath, String)
  fromList = Map.fromList > coerce
  toList   = coerce       > Map.toList

--------------------------------------------------

{-|

@
≡ 'Map.empty'
@

-}

emptyFileTree :: FileTree
emptyFileTree = FileTree Map.empty

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code

--------------------------------------------------

  | FileVariable                -- Mod
  | DirectoryVariable           -- 

--------------------------------------------------



-}
--------------------------------------------------