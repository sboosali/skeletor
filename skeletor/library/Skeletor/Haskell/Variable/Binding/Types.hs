--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Haskell.Variable.Binding.Types where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

--import qualified "text" Data.Text as T
import           "text" Data.Text (Text)

-- NOTE « attoparsec » uses strict « Text ».

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_skeletor hiding (Text)

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-|

-}

-- :: _ -> _
-- = _

newtype TemplateBinding = TemplateBinding

  { getTemplateBinding :: Map Text Text
  }

  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData)


--------------------------------------------------
--------------------------------------------------

{-|

-}

newtype Bindings = Bindings
  { fromBindings ::

     [Binding]

  }
  deriving stock    (Show,Read,Eq,Ord,Generic)
  deriving newtype  (Semigroup,Monoid)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-|

-}

instance IsList Bindings where
  type Item Bindings = (Text, Text)

  fromList = fmap tuple2binding > coerce
  toList   = coerce > fmap binding2tuple

--------------------------------------------------

-- | @= 'defaultBindings'@

instance Default Bindings where
  def = defaultBindings

-- | @≡ []@

defaultBindings :: Bindings
defaultBindings = Bindings []

--------------------------------------------------
--------------------------------------------------

{-|

-}

data Binding = Binding

  { name  :: Text
  , value :: Text
  }

  deriving stock    (Show,Read,Eq,Ord,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-| 

-}

binding2tuple :: Binding -> (Text, Text)
binding2tuple Binding{..} = (name, value)

--------------------------------------------------

{-| 

-}

tuple2binding :: (Text, Text) -> Binding
tuple2binding (name, value) = Binding{..}

--------------------------------------------------
--------------------------------------------------

{-|

e.g. this expression defines a (POSIX) @$PATH@-like syntax:

@
BindingSyntax { separator = ';', operator = '=' }
@

i.e. which parses this tring:

@
"x=1;y=2"
@


-}

data BindingSyntax = BindingSyntax

  { separator :: Char
  , operator  :: Char
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultBindingSyntax'@

instance Default BindingSyntax where
  def = defaultBindingSyntax

--------------------------------------------------
--------------------------------------------------

-- | @≡ 'posixBindingSyntax'@

defaultBindingSyntax :: BindingSyntax
defaultBindingSyntax = posixBindingSyntax

--------------------------------------------------

-- | @BindingSyntax { separator = ':', operator = '=' }@

posixBindingSyntax :: BindingSyntax
posixBindingSyntax = BindingSyntax{..}
  where
    separator = ':'
    operator  = '='

--------------------------------------------------

--------------------------------------------------
--------------------------------------------------
