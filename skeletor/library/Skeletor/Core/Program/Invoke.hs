--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.Program.Invoke where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Core.Program.Types
import Skeletor.Core.Program.Version

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "typed-process" System.Process.Typed as P
import           "typed-process" System.Process.Typed (ProcessConfig)

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------



--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-|

-}

invokeProgram :: Program -> ProgramInput -> IO ProgramOutput
invokeProgram Program{..} ProgramInput{..} = do

  P.

  return ProgramOutput{..}

  where

  =

--------------------------------------------------
--------------------------------------------------

{-|

-}

invokeProgramVersion :: Program -> IO (Maybe Version)
invokeProgramVersion Program{..} = do

  ProgramOutput{..} <- invokeProgram ProgramInput{..}

  let version = parseVersion stdout

  return version

  where

  arguments = [ "--version" ] --TODO  -version  --numeric-version  ...

--------------------------------------------------
--------------------------------------------------
