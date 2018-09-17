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

--------------------------------------------------

{-|

-}

data DelimiterBrackets = DelimiterBrackets
  { open  :: String
  , close :: String
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'wrapDelimiterBrackets'@ 

instance Semigroup DelimiterBrackets where
  (<>) = wrapDelimiterBrackets

{-|

Associative (the @Semigroup@ instance is valid).

**Not** Commutative (being "wrapping" or "nesting", and not appending).

i.e.

@
outermost `wrapDelimiterBrackets` inner `wrapDelimiterBrackets` innermost
@

e.g. @doctest@:

@
>>> :set -XRecordWildCards
>>> let DelimiterBrackets{..} = DelimiterBrackets{ open= "{", close= "}" } <> DelimiterBrackets{ open= "-", close= "-" } <> DelimiterBrackets{ open= "#", close= "#" }
>>> open == ['{', '-', '#' ]
True
>>> close == ['#', '-', '}' ]
True
@

-}

wrapDelimiterBrackets :: DelimiterBrackets -> DelimiterBrackets -> DelimiterBrackets
wrapDelimiterBrackets DelimiterBrackets{open=outerOpen, close=outerClose} DelimiterBrackets{open=innerOpen, close=innerClose} = DelimiterBrackets
  { open  = outerOpen  <> innerOpen
  , close = innerClose <> outerClose
  }

-- {-|
-- -}
-- wrapDelimiterBrackets :: DelimiterBrackets -> DelimiterBrackets -> DelimiterBrackets
-- wrapDelimiterBrackets DelimiterBrackets{open=innerOpen, close=innerClose} DelimiterBrackets{open=outerOpen,close=outerClose} = DelimiterBrackets
--   { open  = outerOpen  <> innerOpen
--   , close = innerClose <> outerClose
--   }

--------------------------------------------------

{-|

-}

dittoDelimiterBrackets :: String -> DelimiterBrackets
dittoDelimiterBrackets s = DelimiterBrackets
  { open  = s
  , close = s
  }

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

data TemplateVariable = TemplateVariable

  { name :: TemplateVariableName
  , kind :: TemplateVariableKind
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

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

data TemplateVariableKind

  = HaskellModuleVariable        -- e.g. Module, ModuleAbbreviation
  | HaskellPackageVariable       -- e.g. Package
  | HaskellModuleAutogenVariable -- e.g. PackageUnderscores

  | CabalComponentVariable      -- e.g. DefaultTarget
  | CabalExecutableVariable     -- e.g. ExecutableName, Program
  | CabalTextVariable           -- e.g. Synopsis, Author, Categories
  | CabalMultilineTextVariable  -- e.g. Description

  | URIVariable                 -- e.g. 
  | URLSegmentVariable          -- e.g. UserName, RepositoryName
  | EmailVariable               -- 
  | CopyrightVariable           -- e.g. Copyright
  
  | PathVariable                -- e.g. ModulePath

  | TextVariable                -- e.g. Author
  | RawTextVariable             -- e.g. 

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-| the lexical style of a variable.

@
@

-}

data TemplateVariableStyle

  = AnyStyle
  | ClassCaseStyle
  | CamelCaseStyle
  | SnakeCaseStyle
  | LispCaseStyle

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultTemplateVariableStyle'@
instance Default TemplateVariableStyle where
  def = defaultTemplateVariableStyle

-- | @= 'AnyStyle'@
defaultTemplateVariableStyle :: TemplateVariableStyle
defaultTemplateVariableStyle = AnyStyle

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code



-}
--------------------------------------------------