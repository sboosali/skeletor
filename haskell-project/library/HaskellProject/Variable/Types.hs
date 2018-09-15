--------------------------------------------------
--------------------------------------------------

{-|



-}

module HaskellProject.Variable.Types where

--------------------------------------------------

-- import qualified "" _ as _

--------------------------------------------------

-- import           "base" _

--------------------------------------------------

import Prelude_haskell_project

--------------------------------------------------
--------------------------------------------------

{-|

@
@

-}

---data KnownVariable

--------------------------------------------------
--------------------------------------------------

{-|

@
@

-}

data MetaConfigurationVariableType

  = WhichLicense
  | WhichVersionControlSystem
  | WhetherPackageInSubdirectory
  | WhichPrelude
 
  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-|

-}

data WhichLicense

  = GPL3License
  | BSD3License

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (Enumerable)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultWhichLicense'@
instance Default WhichLicense where
  def = defaultWhichLicense

-- | @= 'GPL3License'@
defaultWhichLicense :: WhichLicense
defaultWhichLicense = GPL3License

--------------------------------------------------

{-|

-}

data WhichVersionControlSystem

  = GitVCS

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (Enumerable)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultWhichVersionControlSystem'@
instance Default WhichVersionControlSystem where
  def = defaultWhichVersionControlSystem

-- | @= 'GitVCS'@
defaultWhichVersionControlSystem :: WhichVersionControlSystem
defaultWhichVersionControlSystem = GitVCS

--------------------------------------------------

{-|

-}

data WhichPackageDirectory

  = PackageInNamesakeSubdirectory
  | PackageInRootDirectory

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (Enumerable)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultWhichPackageDirectory'@
instance Default WhichPackageDirectory where
  def = defaultWhichPackageDirectory

-- | @= 'PackageInNamesakeSubdirectory'@
defaultWhichPackageDirectory :: WhichPackageDirectory
defaultWhichPackageDirectory = PackageInNamesakeSubdirectory

--------------------------------------------------

{-|

-}

data WhichPrelude

  = BasePrelude
  | SpirosPrelude

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (Enumerable)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultWhichPrelude'@
instance Default WhichPrelude where
  def = defaultWhichPrelude

-- | @= 'BasePrelude'@
defaultWhichPrelude :: WhichPrelude
defaultWhichPrelude = BasePrelude

--------------------------------------------------
--------------------------------------------------

{-|

@
@

-}

data Variable

--------------------------------------------------

{-|

@
@

-}

data VariableStyle

--------------------------------------------------
--------------------------------------------------

{-|

@
@

-}

data ConfigurationVariable

--------------------------------------------------

{-|

@
@

-}

newtype ConfigurationVariableName = ConfigurationVariableName

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

-- | @= 'coerce'@

instance IsString ConfigurationVariableName where
  fromString = coerce

--------------------------------------------------

{-|

@
@

-}

data ConfigurationVariableType

  = StringConfigurationVariable
  | PathConfigurationVariable
  | BooleanConfigurationVariable
  | NumberConfigurationVariable  

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|

@
@

-}

data TemplateVariable

--------------------------------------------------

{-|

@
@

-}

newtype TemplateVariableName = TemplateVariableName

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

-- | @= 'coerce'@

instance IsString TemplateVariableName where
  fromString = coerce

--------------------------------------------------

{-|

@
@

-}

data TemplateVariableType

  = HaskellModuleVariable        -- e.g. Module, ModuleAbbreviation
  | HaskellPackageVariable       -- e.g. Package
  | HaskellModuleAutogenVariable -- e.g. PackageUnderscores

  | CabalComponentVariable      -- e.g. DefaultTarget
  | CabalExecutableVariable     -- e.g. ExecutableName, Program
  | CabalTextVariable           -- e.g. Synopsis, Description
  
  | PathVariable                -- e.g. ModulePath

  | RawTextVariable             -- e.g. TODO
  | CopyrightVariable           -- e.g. Copyright

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code



-}
--------------------------------------------------