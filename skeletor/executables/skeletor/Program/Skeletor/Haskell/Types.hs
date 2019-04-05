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

type Result = (GlobalOptions, Maybe Command)

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
--------------------------------------------------

{-| 'Command' represents this program's subcommands.

-}

data Command

  = CommandCreateProject        CreateProjectOptions
  | CommandDownloadProject      DownloadProjectOptions
  | CommandResolveConfiguration ResolveConfigurationOptions

  | CommandPrintVersion
  | CommandPrintLicense

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-|  

-}

data Options = Options

  { verbosity    :: Verbosity
  , dryrun       :: Dryness
  , printVersion :: Bool
  , printLicense :: Bool
  , license      :: String
  , configpath   :: Maybe FilePath
  , projectpath  :: Maybe FilePath
  , projectname  :: Maybe String
  , subdirectory :: Maybe FilePath
  , bindings     :: [Binding]
  , environment  :: Bindings
  }

  deriving stock    (Show,Read,Eq,Ord,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultOptions'@

instance Default Options where
  def = defaultOptions

{-|

-}

defaultOptions :: Options
defaultOptions = Options{..}
  where

  verbosity    = def
  dryrun       = def
  printVersion = False
  printLicense = False
  license      = def
  configpath   = def
  projectpath  = Nothing
  projectname  = Just defaultProjectName
  subdirectory = Nothing
  bindings     = def
  environment  = def

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

data ResolveConfigurationOptions = ResolveConfigurationOptions

  {
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

{-| How verbose @stdout@ and @stderr@ are.

-}

data Verbosity

  = Concise
  | Verbose

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultVerbosity'@

instance Default Verbosity where
  def = defaultVerbosity

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
-- EOF -------------------------------------------
--------------------------------------------------