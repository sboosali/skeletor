--------------------------------------------------
--------------------------------------------------

{-| Environment Variables whose values are strings
(i.e. 'Text').

Ultimately, environment variables are all "stringly-typed".
(Under @DOS@ and under @SH@. Some shells (e.g.
@Powershell@, @Bash@, @Zsh@, Fish@, etc) have richer types.

-}

module Skeletor.Core.EnvironmentVariable.Text where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Core.EnvironmentVariable.Types

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "text" Data.Text as T

--------------------------------------------------

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------

import "base" System.Environment as Environment

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-| Get the (current) value of the (given) Environment Variable.

-}

getEnvironmentText :: EnvironmentName -> IO (Maybe Text)

getEnvironmentText (EnvironmentName name) = do

  let name' = name & T.unpack

  value' <- Environment.lookupEnv name'

  let value = value' <&> T.pack

  return value

--------------------------------------------------
--------------------------------------------------

{-| Set the (given) "well-named" Environment Variable
to the (given) "well-valued" value.

= Platform-Compatibility

NOTE Because .

-}

setEnvironmentText :: EnvironmentName -> EnvironmentValue -> IO ()

setEnvironmentText (EnvironmentName name) (EnvironmentValue value) = do

  let name'  = T.unpack name
  let value' = T.unpack value

  unsafelySetEnvironmentVariable name' (Just value')

--------------------------------------------------
--------------------------------------------------