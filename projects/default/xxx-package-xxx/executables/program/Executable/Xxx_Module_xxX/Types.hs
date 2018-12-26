
--------------------------------------------------
--------------------------------------------------

{-|



-}

module Executable.Xxx_Module_xxX.Types where

--------------------------------------------------
-- Imports: Internal -----------------------------
--------------------------------------------------

---import qualified "xxx-package-xxx" Executable.Xxx_Module_xxX as Xxx_ModuleAbbreviation_xxX

--------------------------------------------------

import Prelude_exe

--------------------------------------------------
-- Imports: External -----------------------------
--------------------------------------------------

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-|  

-}

data Config = Config

  { verbosity :: Verbosity
  , filepath  :: Maybe FilePath
  , dryrun    :: Dryness
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultConfig'@

instance Default Config where def = defaultConfig

--------------------------------------------------

{-|

-}

defaultConfig :: Config
defaultConfig = Config{..}
  where
  verbosity    = def
  filepath     = def
  dryrun       = def

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

instance Default Verbosity where def = defaultVerbosity

--------------------------------------------------

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