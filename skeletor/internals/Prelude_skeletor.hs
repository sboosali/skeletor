--------------------------------------------------

{- | Custom @Prelude@ for the @skeletor@ package.

-}

module Prelude_skeletor

  ( module EXPORT
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "spiros" Prelude.Spiros        as EXPORT
import "spiros" Prelude.Spiros.Parse  as EXPORT
import "spiros" Prelude.Spiros.Pretty as EXPORT

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------