--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Haskell.Project.Types where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Haskell.Types
--import Skeletor.Haskell.Core.Types

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_skeletor

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

type ProjectName = UnknownOr KnownProjectName

--------------------------------------------------
--------------------------------------------------

{-| 

-}

type ProjectIdentifier = ProjectName --TODO-- 

--------------------------------------------------
--------------------------------------------------

{-| 

-}

data KnownProjectName -- TODO rename -- KnownHaskellProject

  = DefaultProject
  | ComplexProject
  | SimpleProject
  | ForeignProject

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultKnownProject'@

instance Default KnownProjectName where
  def = defaultKnownProjectName

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

-- | @= 'DefaultProject'@

defaultKnownProjectName :: KnownProjectName
defaultKnownProjectName = DefaultProject

--------------------------------------------------
--------------------------------------------------