--------------------------------------------------

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "doctest" Test.DocTest

--------------------------------------------------

import "base" Data.Char
import "base" Data.Foldable
import "base" Prelude

--------------------------------------------------
-- Main ------------------------------------------
--------------------------------------------------

main = do

  printDivider

  doctest (sources ++ flags)

  printDivider

--------------------------------------------------

sources = modules2filepaths "hs" "library" $

  "Skeletor.Types Skeletor.Types.Number"

--------------------------------------------------

flags :: [String]
flags = concat

  [ extensions2flags extensions
  , options
  ]

--------------------------------------------------

options :: [String]
options = concat

  [ include2option <$> includes
  ]

  where

  includes = [ "library", "internals" ]

  include2option = ("-i" <>)

--------------------------------------------------

extensions :: [String]
extensions =

  [ "AutoDeriveTypeable"
  , "BangPatterns"
  , "CPP"
  , "ConstraintKinds"
  , "DataKinds"
  , "DefaultSignatures"
  , "DeriveAnyClass"
  , "DeriveDataTypeable"
  , "DeriveFoldable"
  , "DeriveFunctor"
  , "DeriveGeneric"
  , "DeriveLift"
  , "DeriveTraversable"
  , "DerivingStrategies"
  , "DoAndIfThenElse"
  , "DuplicateRecordFields"
  , "EmptyCase"
  , "EmptyDataDecls"
  , "ExplicitNamespaces"
  , "FlexibleContexts"
  , "FlexibleInstances"
  , "FunctionalDependencies"
  , "GADTs"
  , "GeneralizedNewtypeDeriving"
  , "InstanceSigs"
  , "KindSignatures"
  , "LambdaCase"
  , "MultiParamTypeClasses"
  , "MultiWayIf"
  , "NamedFieldPuns"
  , "NoImplicitPrelude"
  , "PackageImports"
  , "PatternSynonyms"
  , "PostfixOperators"
  , "RankNTypes"
  , "RecordWildCards"
  , "ScopedTypeVariables"
  , "StandaloneDeriving"
  , "TupleSections"
  , "TypeFamilies"
  , "TypeOperators"
  , "UndecidableInstances"
  , "ViewPatterns"
  ]

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

putStringsLine :: [String] -> IO ()
putStringsLine = fmap (const ()) . traverse putStrLn

--------------------------------------------------

printDivider :: IO ()
printDivider = putStrLn "----------------------------------------\n"

--------------------------------------------------

printBlank :: IO ()
printBlank = putStrLn ""

--------------------------------------------------

extensions2flags :: [String] -> [String]
extensions2flags = fmap extension2flag . filterBlanks

extension2flag :: String -> String
extension2flag = ("-X" ++)

--------------------------------------------------

modules2filepaths :: String -> String -> String -> [String]
modules2filepaths extension directory

  = fmap go
  . filterBlanks
  . words

  where

  go s = directory ++ "/" ++ (module2filename s) ++ "." ++ extension

--------------------------------------------------

module2filename :: String -> String
module2filename = replace '.' '/'

--------------------------------------------------

replace
  :: (Functor f, Eq a)
  => a -> a -> f a -> f a

replace a b = fmap go
  where

  go c = if c == a then b else c

--------------------------------------------------

filterBlanks :: [String] -> [String]
filterBlanks = filter (not . areAllCharactersBlank)
  where

  areAllCharactersBlank = all isSpace

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-



-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------