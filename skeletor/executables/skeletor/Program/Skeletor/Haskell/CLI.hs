{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.CLI

  ( 
  ) where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Types

import Program.Skeletor.Haskell.Options
import Program.Skeletor.Haskell.Config
import Program.Skeletor.Haskell.Action
import Program.Skeletor.Haskell.Command

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

import           "base" Data.Maybe
import           "base" Data.Semigroup

--------------------------------------------------

import Prelude_exe

--------------------------------------------------
-- Command-Line Interface ------------------------
--------------------------------------------------

pCommand :: Parser Command
pCommand = empty

  <|> CommandCreateProject        <$> pCreateProjectOptions
  <|> CommandDownloadProject      <$> pDownloadProjectOptions
  <|> CommandResolveConfiguration <$> pResolveConfigurationOptions

--------------------------------------------------
--------------------------------------------------

{-|

-}

pCreateProjectOptions :: Parser CreateProjectOptions
pCreateProjectOptions = CreateProjectOptions

  { globals     :: GlobalOptions
  , location    :: Location
  , destination :: FilePath
  , license     :: License
  }

--------------------------------------------------
--------------------------------------------------

{-|

-}

pDownloadProjectOptions :: Parser DownloadProjectOptions
pDownloadProjectOptions = DownloadProjectOptions

  { globals     :: GlobalOptions
  , location    :: Location
  , destination :: FilePath
  , method      :: FetchBy
  }

--------------------------------------------------
--------------------------------------------------

{-|

-}

-- pResolveConfigurationOptions :: Parser ResolveConfigurationOptions
-- pResolveConfigurationOptions = empty

--------------------------------------------------
--------------------------------------------------