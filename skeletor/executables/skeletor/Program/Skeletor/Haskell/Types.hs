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
  , filepath     :: Maybe FilePath
  , project      :: Maybe String
  , subdirectory :: WhichPackageDirectory
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
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
  filepath     = def
  project      = Just defaultProjectName
  subdirectory = def

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
