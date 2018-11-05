--------------------------------------------------
--------------------------------------------------

{-| Module that re-exports the package API.

= Usage

@import qualified "skeletor" Skeletor.Haskell as Skeletor
@

-}

module Skeletor.Haskell
  (
    -- * Core types (datatypes, typeclasses, and instances).
    module Skeletor.Haskell.Types

    -- * Core definitions.
  , module Skeletor.Haskell.Core

    -- * Configuration variables, Template variables, and their mappings.
  , module Skeletor.Haskell.Variable

    -- * Derived defined.
  , module Skeletor.Haskell.Derived
  
    -- * Read, and Copy, Files and Directories.
  , module Skeletor.Haskell.IO
  
  ) where

--------------------------------------------------

import Skeletor.Haskell.Types
import Skeletor.Haskell.Core
import Skeletor.Haskell.Derived

import Skeletor.Haskell.Variable
import Skeletor.Haskell.IO

--------------------------------------------------
--------------------------------------------------