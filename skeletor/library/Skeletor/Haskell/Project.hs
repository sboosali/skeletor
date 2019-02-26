
--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Haskell.Project

  ( module Skeletor.Haskell.Project.Types
  , module Skeletor.Haskell.Project
  ) where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Haskell.Project.Types

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

import Prelude_skeletor

--------------------------------------------------
-- Printing / Parsing ----------------------------
--------------------------------------------------

-- {-|

-- Inverts 'printBuiltinProjectName'.

-- -}

-- parseProjectName :: (MonadThrow m) => String -> m ProjectName
-- parseProjectName =

--   mkParserFromPrinterWith "KnownProjectName" printBuiltinProjectName allBuiltinProjects

-- --------------------------------------------------

-- {-|

-- -}

-- printProjectName :: ProjectName -> String
-- printProjectName = \case

--   DefaultProject -> "default"

--------------------------------------------------
--------------------------------------------------

{-|

Inverts 'printBuiltinProjectName'.

-}

parseBuiltinProjectName :: (MonadThrow m) => String -> m KnownProjectName
parseBuiltinProjectName =

  mkParserFromPrinterWith "KnownProjectName" printBuiltinProjectName allBuiltinProjects

--------------------------------------------------

{-|

-}

printBuiltinProjectName :: KnownProjectName -> String
printBuiltinProjectName = \case

  DefaultProject -> "default"

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

allBuiltinProjects :: [KnownProjectName]
allBuiltinProjects = genum

--------------------------------------------------

{-|

-}

locateProject :: ProjectName -> FilePath
locateProject = either id locateKnownProject

--------------------------------------------------

{-|

-}

locateKnownProject :: KnownProjectName -> FilePath
locateKnownProject = \case

  DefaultProject -> "/home/sboo/haskell/skeletor/projects/default"  --TODO-- data-files  --TODO-- respect home-directory e.g. ~ or %USER%.

--------------------------------------------------

-- |
--
-- @≡ 'printBuiltinProjectName' <$> 'allBuiltinProjects'@
--

builtinProjectNames :: [String]
builtinProjectNames = printBuiltinProjectName <$> allBuiltinProjects

--------------------------------------------------

-- |
--
-- >>> defaultProjectName
-- "default"
--

defaultProjectName :: String
defaultProjectName = printBuiltinProjectName defaultKnownProjectName

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------