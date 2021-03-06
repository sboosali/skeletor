{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

--------------------------------------------------
--------------------------------------------------

#include <HsSbooMacros.h>

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "doctest" Test.DocTest

--------------------------------------------------

import "base" Prelude

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

{-

$ grep -h -i LANGUAGE -r library/ | sort | uniq | ...

$ cat ... | sort | uniq | xargs | sed -e 's/ / /g'

-}

-- [1] every module in this directory (i.e. `hs-source-dirs`),
-- [2] and all language extensions,
-- whether enabled by default or otherwise used,
-- (i.e. both `default-extensions` and `other-extensions`)
-- EXCEPT those that conflict
-- (e.g. DeriveAnyClass and GeneralizedNewtypeDeriving)

--------------------------------------------------
-- Main ------------------------------------------
--------------------------------------------------

main = do

  printBlank
  printDivider

  putStrLn "Run DocTests within only these files:\n"
  putStringsLine sources

  printBlank

  putStrLn "and with these extensions (/ other compliler options):\n"
  putStringsLine flags

  printBlank
  printDivider

  doctest (sources ++ flags)

  printDivider

--------------------------------------------------

sources = modules2filepaths "hs" "library" $
  "Xxx_Module_xxX Xxx_Module_xxX.Types Xxx_Module_xxX.Core Xxx_Module_xxX.Derived"

--------------------------------------------------

flags = extensions ++ options

  where

  ------------------------------

  extensions = extensions2flags $
      "CPP NoImplicitPrelude ConstraintKinds DataKinds DefaultSignatures DeriveDataTypeable DeriveGeneric ExplicitNamespaces FlexibleContexts FlexibleInstances GADTs KindSignatures LambdaCase RankNTypes ScopedTypeVariables TupleSections TypeFamilies TypeOperators UndecidableInstances PackageImports"
#if HAS_EXTENSION_DerivingStrategies
   ++ " DerivingStrategies"
#endif

  ------------------------------

  options = [
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

extensions2flags :: String -> [String]
extensions2flags = fmap ("-X"++) . words

--------------------------------------------------

modules2filepaths :: String -> String -> String -> [String]
modules2filepaths extension directory = fmap go . words

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
-- EOF -------------------------------------------
--------------------------------------------------