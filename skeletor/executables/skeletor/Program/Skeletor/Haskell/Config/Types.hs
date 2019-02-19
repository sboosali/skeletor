{-# LANGUAGE DuplicateRecordFields #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Config.Types where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Core.Hash

import Skeletor.Haskell.Types hiding (Location)

--------------------------------------------------

import Program.Skeletor.Haskell.Core.Types

--------------------------------------------------

import Program.Skeletor.Haskell.Options.Types
import Program.Skeletor.Haskell.Action.Types

--------------------------------------------------
-- Imports (Extended) ----------------------------
--------------------------------------------------

-- import           "cryptonite" Crypto.Hash (SHA256)

--------------------------------------------------

import qualified "case-insensitive" Data.CaseInsensitive as CI
import           "case-insensitive" Data.CaseInsensitive  ( CI )

--------------------------------------------------
-- Imports (Standard) ----------------------------
--------------------------------------------------

import qualified "text" Data.Text as T
import           "text" Data.Text  ( Text )

--------------------------------------------------
--------------------------------------------------

import Prelude_exe hiding ( Text )

--------------------------------------------------
--------------------------------------------------

{-|  

-}

data Config = Config

  { actions      :: Actions
  , globals      :: GlobalOptions
  , project      :: Project
  }

  deriving stock    (Show,Read,Eq,Ord,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|  

-}

data GlobalOptions = GlobalOptions

  { verbosity    :: Verbosity
  , dryrun       :: Dryness
  }

  deriving stock    (Show,Read,Eq,Ord,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|  

-}

data Project = Project

  { location       :: Location
  , bindings       :: Bindings
  , license        :: SpdxLicenseIdentifier
  , isSubdirectory :: WhichPackageDirectory
  }

  deriving stock    (Show,Read,Eq,Ord,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| Hash for Content-Addressing

-}

--------------------------------------------------

{-| @SHA 256@ hashes.

representation:

* @Base16@-encoded
* case-insensitive

e.g.

@
"1cw5fszffl5pkpa6s6wjnkiv6lm5k618s32sp60kvmvpy7a2v9kg" :: Hash
@

-}

newtype Hash = Hash

  (CI Text)

  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsString Hash where

  fromString = fromString > CI.mk > Hash

--------------------------------------------------



--------------------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------
