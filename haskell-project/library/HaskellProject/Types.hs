
--------------------------------------------------
--------------------------------------------------

{-|



-}

module HaskellProject.Types where

--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import Prelude_haskell_project

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

  = DefaultProject
  
  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)


--------------------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------



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


  
  | FileVariable                -- Mod
  | DirectoryVariable           -- 



-}
--------------------------------------------------