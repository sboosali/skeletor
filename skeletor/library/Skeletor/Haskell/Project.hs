--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

--------------------------------------------------

{- | 

-}

module Skeletor.Haskell.Project

  ( module Skeletor.Haskell.Project.Types
  , module Skeletor.Haskell.Project

  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Skeletor.Haskell.Project.Types

--------------------------------------------------

import "base" Control.Exception (SomeException)

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

--   mkParserFromPrinterWith "BuiltinHaskellProjectName" printBuiltinProjectName allBuiltinHaskellProjectNames

-- --------------------------------------------------

-- {-|

-- -}

-- printProjectName :: ProjectName -> String
-- printProjectName = \case

--   DefaultProject -> "default"

--------------------------------------------------

parseHaskellProjectName :: (MonadThrow m) => String -> m HaskellProjectName
parseHaskellProjectName s = m
  where

  m = eBC & either throwM return

  eBC = eB & either (eC & either throwM return) return

  eB :: Either SomeException BuiltinHaskellProjectName
  eB = parseBuiltinProjectName s

  eC :: Either SomeException CustomHaskellProjectName
  eC = parseCustomProjectName s

--------------------------------------------------

{-|

Inverts 'printBuiltinProjectName'.

-}

parseBuiltinProjectName :: (MonadThrow m) => String -> m BuiltinHaskellProjectName
parseBuiltinProjectName =

  mkParserFromPrinterWith "BuiltinHaskellProjectName" printBuiltinProjectName allBuiltinHaskellProjectNames

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-|

-}

locateProject :: ProjectName -> FilePath
locateProject = either id locateKnownProject

--------------------------------------------------

{-|

-}

locateKnownProject :: BuiltinHaskellProjectName -> FilePath
locateKnownProject = \case

  DefaultProject -> "/home/sboo/haskell/skeletor/projects/default"  --TODO-- data-files  --TODO-- respect home-directory e.g. ~ or %USER%.

--------------------------------------------------

-- |
--
-- @≡ 'printBuiltinProjectName' <$> 'allBuiltinHaskellProjectNames'@
--

builtinProjectNames :: [String]
builtinProjectNames = printBuiltinProjectName <$> allBuiltinHaskellProjectNames

--------------------------------------------------

-- |
--
-- >>> defaultProjectName
-- "default"
--

defaultProjectName :: String
defaultProjectName = printBuiltinProjectName defaultBuiltinHaskellProjectName

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------