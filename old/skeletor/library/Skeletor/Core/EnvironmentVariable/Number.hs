--------------------------------------------------
--------------------------------------------------

{-| Environment Variables whose values are @int@s.

-}

module Skeletor.Core.EnvironmentVariable.Number where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Core.EnvironmentVariable.Types

import Skeletor.Core.EnvironmentVariable.Text

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
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

getEnvironmentNumber :: EnvironmentName -> IO (Maybe Int)

getEnvironmentNumber ev = do

  text <- getEnvironmentText ev

  let int = text >>= parseEnvironmentNumber

  pure int

--------------------------------------------------

parseEnvironmentNumber :: Text -> Maybe Int

parseEnvironmentNumber text = int
  where

  int = go text

  go = T.unpack > readMay

--------------------------------------------------
--------------------------------------------------

setEnvironmentNumber :: EnvironmentName -> Int -> IO ()

setEnvironmentNumber variable int = do

  let text = printEnvironmentNumber int

  setEnvironmentText variable text

  nothing

--------------------------------------------------
--------------------------------------------------

printEnvironmentNumber :: Int -> EnvironmentValue

printEnvironmentNumber int = unsafeEnvironmentValue text
  where

  text = go int

  go = show > T.pack

--------------------------------------------------
--------------------------------------------------