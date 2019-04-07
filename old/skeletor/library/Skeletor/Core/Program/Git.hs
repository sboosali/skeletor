--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.Program.Git where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Core.Program.Types
import Skeletor.Core.Program.Invoke

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "typed-process" System.Process.Typed as P
import           "typed-process" System.Process.Typed (ProcessConfig)

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

{-|

-}

invokeGit :: ProgramInput -> IO ProgramOutput
invokeGit = invokeProgram gitProgram

--TODO-- invokeGit ProgramInput{..} = _

--------------------------------------------------

{-|

-}

gitProgram :: Program
gitProgram = Program{..}

  where
  name = "git"
  path = Nothing

--------------------------------------------------
--------------------------------------------------