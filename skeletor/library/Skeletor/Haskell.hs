--------------------------------------------------
--------------------------------------------------

{-|

Re-export all (public) types and functions in this package.

Example Usage:

@
import qualified "skeletor" Skeletor.Haskell
@

-}

module Skeletor.Haskell
  (
    -- * Core types and instances.
    module Skeletor.Haskell.Types

    -- * Core functions and values.
  , module Skeletor.Haskell.Core

    -- * Configuration variables, Template variables, and their mappings.
  , module Skeletor.Haskell.Variable

    -- * More functions and values.
  , module Skeletor.Haskell.Derived
  
  ) where

--------------------------------------------------

import Skeletor.Haskell.Types
import Skeletor.Haskell.Core
import Skeletor.Haskell.Derived

import Skeletor.Haskell.Variable

--------------------------------------------------
--------------------------------------------------