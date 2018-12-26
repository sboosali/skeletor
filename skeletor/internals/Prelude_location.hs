--------------------------------------------------

{- |

a project-specific custom prelude.

-}

module Prelude_location

  ( module X
  , Text
  ) where

--------------------------------------------------

import "text" Data.Text (Text)

--------------------------------------------------

import "spiros" Prelude.Spiros as X hiding (Text)

--------------------------------------------------