{-# LANGUAGE DuplicateRecordFields #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Types where

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

data Config = Config

  { verbosity    :: Verbosity
  , dryrun       :: Dryness

  , printVersion :: Bool
  , printLicense :: Bool

  , license      :: String

  , configpath   :: Maybe FilePath

  , projectpath  :: Maybe Location
  , projectname  :: Maybe String

  , subdirectory :: WhichPackageDirectory
  , bindings     :: [Binding] --TODO Bindings
  , environment  :: Bindings
  }

  deriving stock    (Show,Read,Eq,Ord,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultConfig'@

instance Default Config where
  def = defaultConfig

{-|

-}

defaultConfig :: Config
defaultConfig = Config{..}
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

{-|  

-}

data Options = Options

  { verbosity    :: Verbosity
  , license      :: License
  }

--------------------------------------------------
--------------------------------------------------

{-|

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

{-|

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

type Assoc k v = [(k, v)]

--------------------------------------------------
--------------------------------------------------

type Location = FilePath

--------------------------------------------------
--------------------------------------------------
