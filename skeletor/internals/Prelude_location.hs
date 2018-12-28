--------------------------------------------------

{- |

a project-specific custom prelude.

-}

module Prelude_location

  ( module X
  , Text

  , module Prelude_location
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "text" Data.Text (Text)

--------------------------------------------------

import "spiros" Prelude.Spiros as X hiding (Text)

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "base" System.Environment as Environment

--------------------------------------------------
--------------------------------------------------

{-| Set the (given) Environment Variable with the (given) 'String'.

= Inputs

inputs:

* 'Nothing' @unset@s the environment variable.
* @('Just' value)@ @set@s the environment variable to @value@.

NOTE this is a partial function.

-}

unsafelySetEnvironmentVariable :: String -> Maybe String -> IO ()

unsafelySetEnvironmentVariable name = \case

  Nothing    -> Environment.unsetEnv name

  Just value -> Environment.setEnv name value

--------------------------------------------------
--------------------------------------------------