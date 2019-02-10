--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.Program.Types where

  ( module Skeletor.Core.Program.Types
  ) where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Core.Program.Types

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

{-|

-}

data Program = Program

  { name :: String          -- the program name.
  , path :: Maybe FilePath  -- filepath (should be absolute).
                           -- if @Nothing@, 'name' is resolved to a 'path' upon invokation.
  }
  

--------------------------------------------------
--------------------------------------------------

{-|

-}

data ProgramInput = ProgramInput

  { subcommand  :: Maybe String  -- ^ Command-Line Subcommand.
  , arguments   :: [String]      -- ^ Command-Line Arguments

  , stdin       :: [String]      -- ^ Standard-Input, by lines.
  , environment :: Environment   -- ^ Environment Variables (overriding parent process's environment).
  --TODO Map EnvironmentName EnvironmentValue
  }
  

--------------------------------------------------
--------------------------------------------------

{-|

-}

data ProgramOutput = ProgramOutput

  { exit   :: ExitCode      -- ^ the Exit-Code which the programed exited with.
  , stdout :: [String]      -- ^ Standard-Output, by lines.
  , stderr :: [String]      -- ^ Standard-Error, by lines.
  }

--------------------------------------------------
--------------------------------------------------