{-# LANGUAGE PackageImports #-}

module Main where

import "cabal-doctest" Distribution.Extra.Doctest ( defaultMainWithDoctests )
import "base"          Prelude

main :: IO ()
main = defaultMainWithDoctests "doc"
