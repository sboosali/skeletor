--------------------------------------------------
--------------------------------------------------

{-| 

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

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

getEnvironmentText :: EnvironmentName -> IO (Maybe Text)

getEnvironmentText (EnvironmentName name) = do

  string <- Environment.lookupEnv name

  let text = string <&> T.pack

  pure text

--------------------------------------------------
--------------------------------------------------

setEnvironmentText :: EnvironmentName -> Text -> IO ()

setEnvironmentText ev text = do

  let string = T.unpack text

  setEnvironmentText string

--------------------------------------------------

setEnvironmentString :: EnvironmentName -> String -> IO ()

setEnvironmentString (EnvironmentName name) value = do

  Environment.setEnv name value

  nothing

--------------------------------------------------
--------------------------------------------------