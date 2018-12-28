--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.EnvironmentVariable.Number where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Core.EnvironmentVariable.Text

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

getEnvironmentNumber :: EnvironmentName -> IO (Maybe Int)

getEnvironmentNumber ev = do

  text <- getEnvironmentText ev

  let int = text <&> parseEnvironmentNumber

  pure int

--------------------------------------------------

parseEnvironmentNumber :: Text -> Maybe Int

parseEnvironmentNumber text = int
  where

  int = go text

  go = 

--------------------------------------------------
--------------------------------------------------

setEnvironmentNumber :: EnvironmentName -> Int -> IO ()

setEnvironmentNumber ev int = do

  let text = show int

  text <- setEnvironmentText ev

  nothing

--------------------------------------------------
--------------------------------------------------