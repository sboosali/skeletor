{-# LANGUAGE DuplicateRecordFields #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Options.Types where

--------------------------------------------------

import Skeletor.Haskell

--------------------------------------------------

-- import           "base" _

--------------------------------------------------

import Prelude_exe

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
  , subdirectory :: WhichPackageDirectory
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
  subdirectory = def
  bindings     = def
  environment  = def

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


--------------------------------------------------
--------------------------------------------------
