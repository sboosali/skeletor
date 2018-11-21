--------------------------------------------------
--------------------------------------------------

{-|



-}

module Skeletor.Haskell.Variable.Types where

--------------------------------------------------

-- import qualified "" _ as _

--------------------------------------------------

-- import           "base" _

--------------------------------------------------

import Prelude_skeletor

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

instance Semigroup DelimiterBrackets where (<>) = wrapDelimiterBrackets

--------------------------------------------------

{-| 

Is Associative (the @Semigroup@ instance being valid).

Is **Not** Commutative (the action being "wrapping" or "nesting", and not appending).

i.e.:

@outermost `wrapDelimiterBrackets` inner `wrapDelimiterBrackets` innermost
@

e.g. (@doctest@):

>>> :set -XRecordWildCards
>>> DelimiterBrackets{..} = DelimiterBrackets{ open= "{", close= "}" } <> DelimiterBrackets{ open= "-", close= "-" } <> DelimiterBrackets{ open= "#", close= "#" }
>>> open == ['{', '-', '#' ]
True
>>> close == ['#', '-', '}' ]
True

-}

wrapDelimiterBrackets :: DelimiterBrackets -> DelimiterBrackets -> DelimiterBrackets
wrapDelimiterBrackets DelimiterBrackets{open=outerOpen, close=outerClose}
                      DelimiterBrackets{open=innerOpen, close=innerClose} =

  DelimiterBrackets
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

e.g. (@doctest@):

>>> :set -XRecordWildCards
>>> DelimiterBrackets{..} = dittoDelimiterBrackets "=="
>>> open == close
True

-}

dittoDelimiterBrackets :: String -> DelimiterBrackets
dittoDelimiterBrackets s = DelimiterBrackets

  { open  = s
  , close = s
  }

--------------------------------------------------
--------------------------------------------------

{-| These variables aren't /interpolated/, they represent arbitrary template fragments
(or choose between templates in separate files).

In particular:

* 'WhichLicenese': 
* 'WhichVersionControlSystem': 
* 'WhetherPackageInSubdirectory': 
* 'WhichPrelude': 

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
  | BSD4License
  | BSD3License
  | BSD2License
  | MITLicense

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (Enumerable)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultWhichLicense'@
instance Default WhichLicense where def = defaultWhichLicense

--------------------------------------------------

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
instance Default WhichVersionControlSystem where def = defaultWhichVersionControlSystem

--------------------------------------------------

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
instance Default WhichPackageDirectory where def = defaultWhichPackageDirectory

--------------------------------------------------

-- | @= 'PackageInNamesakeSubdirectory'@
defaultWhichPackageDirectory :: WhichPackageDirectory
defaultWhichPackageDirectory = PackageInNamesakeSubdirectory

--------------------------------------------------

{-|

-}

data WhichPrelude

  = ImplicitPrelude             -- ^ @Prelude@ (the official one).
  | BasePrelude                 -- ^ Re-Export as much of @base@ as possible.
  | SpirosPrelude               -- ^ The library author's custom prelude (from the @spiros@ package).

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (Enumerable)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultWhichPrelude'@
instance Default WhichPrelude where def = defaultWhichPrelude

--------------------------------------------------

-- | @= 'ImplicitPrelude'@
defaultWhichPrelude :: WhichPrelude
defaultWhichPrelude = ImplicitPrelude

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

--------------------------------------------------

-- | @= 'coerce'@
instance IsString ConfigurationVariableName where fromString = coerce

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

--------------------------------------------------

-- | @= 'coerce'@
instance IsString TemplateVariableName where fromString = coerce

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

{-| The lexical style of a variable.

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
instance Default TemplateVariableStyle where def = defaultTemplateVariableStyle

--------------------------------------------------

-- | @= 'AnyStyle'@
defaultTemplateVariableStyle :: TemplateVariableStyle
defaultTemplateVariableStyle = AnyStyle

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code



-}
--------------------------------------------------