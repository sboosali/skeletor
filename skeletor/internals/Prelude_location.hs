--------------------------------------------------

{- |

a project-specific custom prelude.

-}

module Prelude_location

  ( module EXPORT
  , module Prelude_location

  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "spiros" Prelude.Spiros as EXPORT

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--import qualified "base" Control.Exception as E

--------------------------------------------------

import "base" System.Environment as Environment

--------------------------------------------------
-- Definitions -----------------------------------
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
-- EOF -------------------------------------------
--------------------------------------------------