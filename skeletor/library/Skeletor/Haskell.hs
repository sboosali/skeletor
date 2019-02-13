--------------------------------------------------
--------------------------------------------------

{-| Module that re-exports the package API.

= Usage

@import qualified "skeletor" Skeletor.Haskell as Skeletor.Haskell
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

  --   -- * Read (or Copy) and parse Project Files.
  -- , module Skeletor.Haskell.IO

    -- * Find Files (like @findutils@' @find@ program).
  , module Skeletor.Haskell.Find

    -- * Scripting (via the @turtle@ package).
  , module Skeletor.Haskell.Turtle

    -- * Licenses (@SPDX).
  , module Skeletor.Haskell.License

  ) where

--------------------------------------------------

import Skeletor.Haskell.Types
import Skeletor.Haskell.Core
import Skeletor.Haskell.Derived
import Skeletor.Haskell.Variable
--import Skeletor.Haskell.IO
import Skeletor.Haskell.Find
import Skeletor.Haskell.Turtle

--------------------------------------------------
--------------------------------------------------