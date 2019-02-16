{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE ApplicativeDo     #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Config where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Types
--import Program.Skeletor.Haskell.Config.Types

import Program.Skeletor.Haskell.Action
import Program.Skeletor.Haskell.Options

--------------------------------------------------

import Skeletor.Haskell
import Skeletor.Haskell.Variable.Binding

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "attoparsec" Data.Attoparsec.Text as A

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "text" Data.Text as T
import           "text" Data.Text (Text)

-- NOTE « attoparsec » uses strict « Text ».

--------------------------------------------------
--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative as P

--------------------------------------------------

-- import           "base" _

--------------------------------------------------

import Prelude_exe

--------------------------------------------------
--------------------------------------------------

{-| 

-}

toConfig :: (MonadThrow m) => Options -> m Config
toConfig options@Options{ location = location', ..} = do

  bindings <- return (environment <> (Bindings bindings))

  

  license <- readSpdxLicenseIdentifier license

  actions <- toActions options

  let globals = GlobalOptions
        { dryrun
        , verbosity
        }

  let project = Project
        { location = location'
        , license
        , subdirectory
        , bindings = bindings
        }

  let config = Config
        { actions
        , globals
        , project
        }

  return config

--------------------------------------------------
--------------------------------------------------
