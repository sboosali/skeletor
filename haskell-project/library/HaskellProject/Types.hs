
--------------------------------------------------
--------------------------------------------------

{-|



-}

module HaskellProject.Types where

--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import Prelude_haskell_project

--------------------------------------------------
--------------------------------------------------

type UnknownOr = Either String    -- TODO

--------------------------------------------------
--------------------------------------------------

{-| 

-}

type ProjectIdentifier = UnknownOr KnownProject

--------------------------------------------------

{-| 

-}

data KnownProject

  = DefaultProject
  
  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)


--------------------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------



--------------------------------------------------
--------------------------------------------------

{-|

-}

newtype FileTree = FileTree

  (Map FilePath String)         -- TODO Text

  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData)

instance IsList FileTree where
  type Item FileTree = (FilePath, String)
  fromList = Map.fromList > coerce
  toList   = coerce       > Map.toList

--------------------------------------------------

{-|

@
≡ 'Map.empty'
@

-}

emptyFileTree :: FileTree
emptyFileTree = FileTree Map.empty

--------------------------------------------------
--------------------------------------------------

{-|

@
@

-}

data KnownVariable


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

data MetaConfigurationVariableType

  = WhichLicense
  | WhichVersionControlSystem
 
  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

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

  = HaskellModuleVariable        -- Module, ModuleAbbreviation
  | HaskellPackageVariable       -- Package
  | HaskellModuleAutogenVariable -- PackageUnderscores

  | CabalComponentVariable      -- DefaultTarget
  | CabalExecutableVariable     -- ExecutableName, Program
  | CabalTextVariable           -- Synopsis, Description
  
  | PathVariable                -- ModulePath

  | RawTextVariable             -- 
  | CopyrightVariable           -- Copyright

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------



--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code


  
  | FileVariable                -- Mod
  | DirectoryVariable           -- 



-}
--------------------------------------------------