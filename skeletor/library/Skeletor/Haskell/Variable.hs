--------------------------------------------------
--------------------------------------------------

{-| Variables within templates.

(Re-export all types and functions of this module's submodules.)

-}

module Skeletor.Haskell.Variable
  (
    -- * (Types and instances.)
    module Skeletor.Haskell.Variable.Types

    -- * (Functions and values.)
  , module Skeletor.Haskell.Variable.Values
  
    -- * Template Variable Bindings (a.k.a. Substitutions).
  , module Skeletor.Haskell.Variable.Binding
  
  ) where

--------------------------------------------------

import Skeletor.Haskell.Variable.Types
import Skeletor.Haskell.Variable.Values
import Skeletor.Haskell.Variable.Binding

--------------------------------------------------
--------------------------------------------------