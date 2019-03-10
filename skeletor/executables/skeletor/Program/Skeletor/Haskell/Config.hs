{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Config

  ( toConfig
  ) where

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

import           "base" Data.Maybe
import           "base" Data.Semigroup

--------------------------------------------------

import Program.Skeletor.Haskell.Prelude

--------------------------------------------------
--------------------------------------------------

{-| 

-}

toConfig :: (MonadThrow m) => Options -> m Config
toConfig options@Options{..} = do

  bindings' <- return (environment <> (Bindings bindings))

  let location = mkLocation subdirectory projectpath projectname

  license' <- parseLicense license

  actions <- toActions options

  let globals = GlobalOptions
        { dryrun
        , verbosity
        }

  let project = Project
        { location
        , license = license'
        , isSubdirectory = def
        , bindings = bindings'
        }

  let config = Config
        { actions
        , globals
        , project
        }

  return config

--------------------------------------------------
--------------------------------------------------

{-| 

-}

mkLocation subdirectory projectpath projectname = fromMaybes LocationStdin

  [ LocationPath <$> (projectpath)
  , LocationPath <$> (fromProjectName =<< projectname)
  ]

  where

  fromMaybes :: a -> [Maybe a] -> a
  fromMaybes x = catMaybes > listToMaybe > fromMaybe x

  fromProjectName :: String -> Maybe FilePath
  fromProjectName = parseBuiltinProjectName >=> returning locateKnownProject

--------------------------------------------------
--------------------------------------------------
