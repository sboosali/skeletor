--------------------------------------------------

{- | Types of template\/configuration variables.

== Types

* `Boolean` — “stringly-typed” bools (a.k.a flags).
* `Number`  — “stringly-typed” numbers (i.e. integers and\/or reals).
* `Path`    — @newtyp@ed "FilePaths".

-}

module Skeletor.Variable.Types

  ( module Skeletor.Variable.Types.Boolean
  , module Skeletor.Variable.Types.Number
  , module Skeletor.Variable.Types.Path

  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import Skeletor.Variable.Types.Boolean
import Skeletor.Variable.Types.Number
import Skeletor.Variable.Types.Path

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------