{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DuplicateRecordFields #-}

--------------------------------------------------
--------------------------------------------------

{-|

-}

module Program.Skeletor.Haskell.Types

  ( module Program.Skeletor.Haskell.Types
  , module Skeletor.Haskell.Types
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import Skeletor.Haskell
import Skeletor.Haskell.Types
--import Skeletor.Haskell.Project

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Prelude

--------------------------------------------------

-- import           "cryptonite" Crypto.Hash (SHA256)

--------------------------------------------------

import qualified "case-insensitive" Data.CaseInsensitive as CI
import           "case-insensitive" Data.CaseInsensitive  ( CI )

--------------------------------------------------

import qualified "generic-lens" Data.Generics.Product as G
--import qualified "generic-lens" Data.Generics.Sum     as G

--------------------------------------------------

import           "text" Data.Text  ( Text )

--------------------------------------------------

import           "base" Control.Exception
import           "base" System.Exit

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

type URL = String               -- TODO

--------------------------------------------------
--------------------------------------------------

{-| Options and subcommand.

-}

type Result = (GlobalOptions, Command)

--------------------------------------------------
--------------------------------------------------

{-| @ConstraintKind@ for a @{ 'globals' :: 'GlobalOptions' } field.

-}

type HasField_GlobalOptions s =
  ( G.HasField' "globals" s GlobalOptions
  , Generic s
  )

--------------------------------------------------
--------------------------------------------------

{-| 

-}

data GlobalOptions = GlobalOptions

  { verbosity    :: Verbosity
  , dryrun       :: Dryness
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic,Lift)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultGlobalOptions'@

instance Default GlobalOptions where
  def = defaultGlobalOptions

--------------------------------------------------

-- | @'defaultVerbosity'@ and @'defaultDryness'@

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions = GlobalOptions

  { verbosity = def
  , dryrun    = def
  }

--------------------------------------------------
--------------------------------------------------

{-| 'Command' represents this program's subcommands.

-}

data Command

  = CommandCreateProject        CreateProjectOptions
  | CommandDownloadProject      DownloadProjectOptions
  | CommandResolveConfiguration ResolveConfigurationOptions

  | CommandPrintVersion  GlobalOptions
  | CommandPrintLicense  GlobalOptions
  | CommandPrintExamples GlobalOptions

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data CreateProjectOptions = CreateProjectOptions

  { globals     :: GlobalOptions
  , location    :: Location
  , destination :: FilePath
  , license     :: License
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data CreateProject = CreateProject

  { location    :: Location
  , destination :: FilePath
  , license     :: License
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data DownloadProjectOptions = DownloadProjectOptions

  { globals     :: GlobalOptions
  , location    :: Location
  , destination :: FilePath
  , method      :: FetchBy
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data DownloadProject = DownloadProject

  { location    :: Location
  , destination :: FilePath
  , method      :: FetchBy
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data ProjectCreated = ProjectCreated

  { status :: Status
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data ProjectDownloaded = ProjectDownloaded

  { status :: Status
  , path   :: FilePath
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
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

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-| 

-}

data ResolveConfigurationOptions = ResolveConfigurationOptions

  { globals     :: GlobalOptions
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-| 

-}

data ResolveConfiguration = ResolveConfiguration

  { extraConfig :: Configuration
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| 

-}

data ConfigurationResolved = ConfigurationResolved

  { status :: Status
  , config :: Configuration
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| 

-}

data Configuration = Configuration

  { settings :: Settings
  , bindings :: Bindings
  
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

instance Semigroup Configuration where

  Configuration{} <> Configuration{} = Configuration{}

--------------------------------------------------

instance Monoid Configuration where

  mempty = Configuration{}

--------------------------------------------------

-- | @= 'defaultConfiguration'@

instance Default Configuration where
  def = defaultConfiguration

--------------------------------------------------

-- | @= 'Configuration'@

defaultConfiguration :: Configuration
defaultConfiguration = Configuration{}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

data Settings = Settings

  { extraRepositories :: () -- [RepositoryLocation]
  
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| How verbose @stdout@ and @stderr@ are.

-}

data Verbosity

  = Silent
  | Concise
  | Verbose
  | Vociferous

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultVerbosity'@

instance Default Verbosity where
  def = defaultVerbosity

--------------------------------------------------

-- | @= 'Concise'@

defaultVerbosity :: Verbosity
defaultVerbosity = Concise

--------------------------------------------------
--------------------------------------------------

{-| Whether effects are enabled or disabled.

-}

data Dryness

  = DryRun
  | TrueRun

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultDryness'@

instance Default Dryness where def = defaultDryness

--------------------------------------------------

-- | @= 'TrueRun'@

defaultDryness :: Dryness
defaultDryness = TrueRun

--------------------------------------------------
--------------------------------------------------

{-|

-}

data Status

  = Success
  | Failure

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

instance Exception Status where

--------------------------------------------------
--------------------------------------------------

data Location

  = LocationStdin
  | LocationPath  FilePath
  | LocationURI   URI

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

-- deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| How to fetch (a.k.a download) a location.

TODO When fetching @URL@s, 'FetchBy' specifies an HttpTransport@.

-}

data FetchBy

  = FetchByHaskell
  | FetchByCurl
  | FetchByWget

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultFetchBy'@

instance Default FetchBy where
  def = defaultFetchBy

--------------------------------------------------

-- | @= 'FetchByHaskell'@

defaultFetchBy :: FetchBy
defaultFetchBy = FetchByHaskell

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

{-|  

-}

data CommandFailure = CommandFailure

  { exitcode :: !ExitCode
  , stderr   :: !String
  }

  deriving stock    (Show,Read,Eq,Ord{-,Lift-},Generic)
  deriving anyclass (NFData{-,Hashable-})

--------------------------------------------------

-- | 

instance Exception CommandFailure where

  displayException = show --TODO-- 

--------------------------------------------------

-- | @= 'defaultCommandFailure'@

instance Default CommandFailure where
  def = defaultCommandFailure

--------------------------------------------------

{-|

-}

defaultCommandFailure :: CommandFailure
defaultCommandFailure = CommandFailure{..}
  where

  exitcode = ExitFailure 1
  stderr   = ""

--------------------------------------------------
-- Accessors -------------------------------------
--------------------------------------------------

addGlobalOptionsTo :: (HasField_GlobalOptions s) => GlobalOptions -> s -> s
addGlobalOptionsTo globals1 options = setGlobalOptions globals options
  where

  globals = globals1 `mergeGlobalOptions` globals2

  globals2 = getGlobalOptions options

--------------------------------------------------

getGlobalOptions :: (HasField_GlobalOptions s) => s -> GlobalOptions
getGlobalOptions = G.getField @"globals"

--------------------------------------------------

setGlobalOptions :: (HasField_GlobalOptions s) => GlobalOptions -> s -> s
setGlobalOptions = G.setField @"globals"

-------------------------------------------------- 

mergeGlobalOptions :: GlobalOptions -> GlobalOptions -> GlobalOptions
mergeGlobalOptions GlobalOptions{ verbosity = verbosity1, dryrun = dryrun1 } GlobalOptions{ verbosity = verbosity2, dryrun = dryrun2 }

  = GlobalOptions{ verbosity, dryrun }

  where

  verbosity = min verbosity1 verbosity2 -- prefer « Concise »

  dryrun    = min dryrun1 dryrun2       -- prefer « DryRun »

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

-- Cabal's Verbosity Levels:
-- 
-- data VerbosityLevel = Silent | Normal | Verbose | Deafening
-- 
-- -- We shouldn't print /anything/ unless an error occurs in silent mode
-- silent :: Verbosity
-- 
-- -- Print stuff we want to see by default
-- normal :: Verbosity
-- 
-- -- Be more verbose about what's going on
-- verbose :: Verbosity
-- 
-- -- Not only are we verbose ourselves (perhaps even noisier than when
-- -- being "verbose"), but we tell everything we run to be verbose too
-- deafening :: Verbosity
-- 

--------------------------------------------------

-- 

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------