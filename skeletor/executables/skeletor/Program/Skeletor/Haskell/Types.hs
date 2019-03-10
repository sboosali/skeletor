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
import Skeletor.Haskell.Project

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------

-- import           "cryptonite" Crypto.Hash (SHA256)

--------------------------------------------------

import qualified "case-insensitive" Data.CaseInsensitive as CI
import           "case-insensitive" Data.CaseInsensitive  ( CI )

--------------------------------------------------

import qualified "generic-lens" Data.Generics.Product as G
import qualified "generic-lens" Data.Generics.Sum     as G

--------------------------------------------------

import           "text" Data.Text  ( Text )

--------------------------------------------------

import           "base" Control.Exception
import           "base" System.Exit

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

type URL = String

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

type ResolveConfigurationOptions = ()

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
--------------------------------------------------