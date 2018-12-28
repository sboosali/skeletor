--------------------------------------------------
--------------------------------------------------

{-| Environment Variables whose values are filepaths
(i.e. 'FilePath' [TODO: rich type]).

-}

module Skeletor.Core.EnvironmentVariable.Path where

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

type Path = Text                -- TODO

-- newtype Path = Path Strict.ByteString

--------------------------------------------------
--------------------------------------------------

{-| Get the (current) value of the (given) Environment Variable.

-}

getEnvironmentPath :: EnvironmentName -> IO (Maybe Path)

getEnvironmentPath (EnvironmentName name) = do

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

setEnvironmentPath :: EnvironmentName -> EnvironmentValue -> IO ()

setEnvironmentPath (EnvironmentName name) (EnvironmentValue value) = do

  let name'  = T.unpack name
  let value' = T.unpack value

  unsafelySetEnvironmentVariable name' (Just value')

--------------------------------------------------
--------------------------------------------------