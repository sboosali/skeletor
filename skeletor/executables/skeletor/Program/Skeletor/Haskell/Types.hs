{-# LANGUAGE DuplicateRecordFields #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Types

  ( module Program.Skeletor.Haskell.Core.Types
  , module Program.Skeletor.Haskell.Options.Types
  , module Program.Skeletor.Haskell.Action.Types
  , module Program.Skeletor.Haskell.Config.Types
  , module Skeletor.Haskell.Types
  , module Program.Skeletor.Haskell.Types
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import Skeletor.Haskell.Types

import Program.Skeletor.Haskell.Core.Types
import Program.Skeletor.Haskell.Options.Types
import Program.Skeletor.Haskell.Action.Types
import Program.Skeletor.Haskell.Config.Types

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------

import           "base" Control.Exception
import           "base" System.Exit

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------



--------------------------------------------------
--------------------------------------------------

{-|  

-}

data CommandFailure = CommandFailure

  { exitcode :: !ExitCode
  , stderr   :: !String
  }

  deriving stock    (Show,Read,Eq,Ord{-,Lift-},Generic)
  deriving anyclass (NFData{-,Hashable-})

--------------------------------------------------

-- | 

instance Exception CommandFailure where

  displayException = show --TODO-- 

--------------------------------------------------

-- | @= 'defaultCommandFailure'@

instance Default CommandFailure where
  def = defaultCommandFailure

--------------------------------------------------

{-|

-}

defaultCommandFailure :: CommandFailure
defaultCommandFailure = CommandFailure{..}
  where

  exitcode = ExitFailure 1
  stderr   = ""

--------------------------------------------------
--------------------------------------------------