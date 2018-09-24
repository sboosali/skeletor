--------------------------------------------------
--------------------------------------------------

{-|

Re-export all (public) types and functions in this package.

Example Usage:

@
import qualified "xxx-package-xxx" Xxx_Module_xxX as Xxx_ModuleAbbreviation_xxX
@

See the @example-xxx-package-xxx@ executable component ( under @<./executables\/example\/Example\/Xxx_Module_xxX.hs>@ ).

-}

module Xxx_Module_xxX
  (
    -- * Core types and instances.
    module Xxx_Module_xxX.Types

    -- * Core functions and values.
  , module Xxx_Module_xxX.Core

    -- * More functions and values.
  ,  module Xxx_Module_xxX.Derived

--, module  Xxx_Module_xxX
  ) where

--------------------------------------------------

import Xxx_Module_xxX.Types
import Xxx_Module_xxX.Core
import Xxx_Module_xxX.Derived

--------------------------------------------------
--------------------------------------------------

{-|


-}



--------------------------------------------------
--------------------------------------------------