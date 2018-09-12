--------------------------------------------------
--------------------------------------------------

{-|

Re-export all (public) types and functions in this package.

Example Usage:

@
import qualified "__PACKAGE__" __MODULE__ as __MODULE_ABBREVIATION__
@

-}

module __MODULE__
  (
    -- * Core types and instances.
    module __MODULE__.Types

    -- * Core functions and values.
    module __MODULE__.Core

    -- * More functions and values.
    module __MODULE__.Derived

--, module  __MODULE__
  ) where

--------------------------------------------------

import __MODULE__.Types
import __MODULE__.Core
import __MODULE__.Derived

--------------------------------------------------
--------------------------------------------------

{-|


-}



--------------------------------------------------
--------------------------------------------------