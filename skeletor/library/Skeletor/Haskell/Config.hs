--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Haskell.Config.Types where

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

data KnownProjectConfig -- TODO rename -- KnownHaskellProject

  = DefaultProjectConfig   UniversalProjectConfig
  | ComplexProjectConfig   UniversalProjectConfig
  | SimpleProjectConfig    UniversalProjectConfig
  | ForeignProjectConfig   UniversalProjectConfig

-- --| Project UniversalProjectConfig -- ^ 
  
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultKnownProject'@

instance Default KnownProjectConfig where
  def = defaultKnownProjectConfig

--------------------------------------------------
--------------------------------------------------

data UniversalProjectConfig = UniversalProjectConfig

  { components :: CabalComponents
  , license    :: License
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultKnownProject'@

instance Default UniversalProjectConfig where
  def = defaultUniversalProjectConfig

--------------------------------------------------
--------------------------------------------------

{-|

-}

newtype CabalComponents = CabalComponents

  [CabalComponent]

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsList CabalComponents where

  type Item CabalComponents = CabalComponent

  fromList = nub > coerce
  toList   = coerce

--------------------------------------------------
--------------------------------------------------

{-| The "Components" of a Cabal package are:

* @libs@, @libraries@
* @flibs@, @foreign-libraries@
* @exes@, @executables@
* @tests@
* @benches@, @benchmarks@;

-}

data CabalComponent

  = CabalLibrary  -- TODO primary | named | private
  | CabalForeignLibrary
  | CabalExecutable
  | CabalTestSuite
  | CabalBenchmark

--  --| Cabal

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

-- | @= ('DefaultHaskellProject' 'defaultUniversalProjectConfig')@

defaultKnownProjectConfig :: KnownProjectConfig
defaultKnownProjectConfig = DefaultProjectConfig defaultUniversalProjectConfig

--------------------------------------------------

{- |

'defaultLicense' for a @library@.
'defaultFLOSSLicense' for @executables@.

-}

defaultUniversalProjectConfig :: UniversalProjectConfig
defaultUniversalProjectConfig = UniversalProjectConfig{..}
  where

  components = allCabalComponents
  license    = defaultLicense

--------------------------------------------------
--------------------------------------------------

defaultCabalComponents :: CabalComponents
defaultCabalComponents = allCabalComponents

--------------------------------------------------
--------------------------------------------------

allCabalComponents :: CabalComponents
allCabalComponents = CabalComponents (constructors @CabalComponent)

--------------------------------------------------
--------------------------------------------------