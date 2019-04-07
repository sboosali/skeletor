--------------------------------------------------
--------------------------------------------------

{-| Environment Variables

-}

module Skeletor.Core.EnvironmentVariable

  (

    -- *
    module Skeletor.Core.EnvironmentVariable

    -- * Valid names for Environment Variables (see 'EnvironmentName').
  , module Skeletor.Core.EnvironmentVariable.Name

    -- * Valid values that Environment Variables can have (see 'EnvironmentValue').
  , module Skeletor.Core.EnvironmentVariable.Value

    -- * Environment Variables that are strings (the default, and majority).
  , module Skeletor.Core.EnvironmentVariable.Text

    -- * Environment Variables that are filepaths.
  , module Skeletor.Core.EnvironmentVariable.Path

    -- * Environment Variables that are lists (e.g. @$PATH@).
  , module Skeletor.Core.EnvironmentVariable.List

    -- * Environment Variables that are numbers (e.g. @$PORT@).
  , module Skeletor.Core.EnvironmentVariable.Number

    -- *
  , module Skeletor.Core.EnvironmentVariable.Types

  ) where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Core.EnvironmentVariable.Types

import Skeletor.Core.EnvironmentVariable.Name
import Skeletor.Core.EnvironmentVariable.Value

import Skeletor.Core.EnvironmentVariable.Text
import Skeletor.Core.EnvironmentVariable.Path
import Skeletor.Core.EnvironmentVariable.List
import Skeletor.Core.EnvironmentVariable.Number

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-| A valid name for an environment-variable.

See 'EnvironmentName'.

-}

type EN = EnvironmentName

--------------------------------------------------

{-| A valid value that an environment-variable can be set to.

See 'EnvironmentValue'.

-}

type EV = EnvironmentValue

--------------------------------------------------
--------------------------------------------------