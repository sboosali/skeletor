{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------

{-# LANGUAGE BlockArguments #-}

--------------------------------------------------
--------------------------------------------------

{-| Macros.

(a Haskell Macro is: a Template-Haskell Splice and/or a Quasi-Quoter).

-}

module Program.Skeletor.Haskell.Macros

  ( currentGitCommit
  , currentTimestamp
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Prelude

--------------------------------------------------
--------------------------------------------------

import qualified "template-haskell" Language.Haskell.TH as TH
import           "template-haskell" Language.Haskell.TH        (Q)
import           "template-haskell" Language.Haskell.TH.Syntax (TExp)

--------------------------------------------------

import qualified "process" System.Process as P

--------------------------------------------------
-- Macros ----------------------------------------
--------------------------------------------------

{- | Invokes @git@.

e.g.:

@
$ git rev-parse --verify HEAD
0c1ef1bfa6a978743252bf738117af265c4e6cc2
@

-}

currentGitCommit :: Q (TExp String)
currentGitCommit = do

  string <- io $ shell "git rev-parse --verify HEAD"

  hexp <- [|| string ||]
  return hexp

  -- « hexp » abbreviates "Haskell EXPression".

--------------------------------------------------

{- | Invokes @date@.

e.g.:

@
$ date +%Y-%m-%d-%Hh-%Mm
2019-04-07-02h-55m
@

-}

currentTimestamp :: Q (TExp String)
currentTimestamp = do

  string <- io $ shell "date +%Y-%m-%d-%Hh-%Mm"

  [|| string ||]

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

shell :: String -> IO String
shell cmdln = do

  stdout <- P.readCreateProcess (P.shell cmdln) stdin

  return (strip stdout)

  where

  stdin = ""

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------