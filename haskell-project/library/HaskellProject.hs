--------------------------------------------------
--------------------------------------------------

{-|

Re-export all (public) types and functions in this package.

Example Usage:

@
import qualified "haskell-project" HaskellProject
@

-}

module HaskellProject
  (
    -- * Core types and instances.
    module HaskellProject.Types

    -- * Core functions and values.
  , module HaskellProject.Core

    -- * Configuration variables, Template variables, and their mappings.
  , module HaskellProject.Variable

    -- * More functions and values.
  , module HaskellProject.Derived
  
  ) where

--------------------------------------------------

import HaskellProject.Types
import HaskellProject.Core
import HaskellProject.Derived

import HaskellProject.Variable

--------------------------------------------------
--------------------------------------------------