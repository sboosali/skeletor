--------------------------------------------------
--------------------------------------------------

{-| @URI@s, with different contents and (uri-)schemes.

(Re-export all types and functions of this module's submodules.)

-}

module Skeletor.Core.URI
  (
    -- * (Types and instances.)
    module Skeletor.Core.URI.Types

    -- * "Reading" (downloading) different 'URI's.
  , module Skeletor.Core.URI.Read
  ) where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Core.URI.Types
import Skeletor.Core.URI.Read

--------------------------------------------------
--------------------------------------------------
