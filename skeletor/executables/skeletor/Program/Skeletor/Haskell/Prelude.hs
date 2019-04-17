
--------------------------------------------------

{- | Ccustom prelude for @haskell-skeletor@.

-}

module Program.Skeletor.Haskell.Prelude

    ( module EXPORTS
    ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "spiros" Prelude.Spiros       as EXPORTS hiding (Text)
import "spiros" Prelude.Spiros.Print as EXPORTS
import "spiros" Prelude.Spiros.Parse as EXPORTS

--------------------------------------------------

import "modern-uri" Text.URI         as EXPORTS        (URI)

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------