--------------------------------------------------
--------------------------------------------------

{-| 'Directory' is a (simplified, portable) representation of a hiearchy of files.

'Directory' is polymorphic, specializations of which includes:

* (actual) filesystem directories
* archives.
* tarball.
* sets of paths.

(Re-export all types and functions of this module's submodules.)

-}

module Skeletor.Core.Directory
  (
    -- * Types and instances.
    module Skeletor.Core.Directory.Types

    -- * Functions and values.
  , module Skeletor.Core.Directory.Values
  
  ) where

--------------------------------------------------

import Skeletor.Core.Directory.Types
import Skeletor.Core.Directory.Values

--------------------------------------------------
--------------------------------------------------
