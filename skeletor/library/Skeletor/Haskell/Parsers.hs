{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Skeletor.Haskell.Parsers

  ( pURI
  , pFetchBy
  , pLicense
  , pLocation
  , pProject

  ) where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Skeletor.Haskell
import Skeletor.Haskell.License
import Skeletor.Haskell.Variable.Binding

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "attoparsec" Data.Attoparsec.Text as A

--------------------------------------------------

import qualified "modern-uri" Text.URI as URI
import           "modern-uri" Text.URI (URI)

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "text" Data.Text as T
import           "text" Data.Text (Text)

-- NOTE « attoparsec » uses strict « Text ».

--------------------------------------------------
--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative      as Optparse
import qualified "optparse-applicative" Options.Applicative.Help as Optparse hiding (fullDesc)

--------------------------------------------------

import           "base" Data.Maybe
import           "base" Data.Semigroup

--------------------------------------------------

import Program.Skeletor.Haskell.Prelude

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

--------------------------------------------------
-- Parsers ---------------------------------------
--------------------------------------------------

pURI :: P.ReadM URI
pURI = _

--------------------------------------------------

pProject :: P.ReadM Project
pProject = _

--------------------------------------------------

pLicense :: P.ReadM License
pLicense = _

--------------------------------------------------

pLocation :: P.ReadM Location
pLocation = _

--------------------------------------------------

pFetchBy :: P.ReadM FetchBy
pFetchBy = _

--------------------------------------------------

rBindings :: P.ReadM Bindings
rBindings = P.eitherReader (A.parseOnly p . T.pack)
  where

  p = pBindings bindingSyntax

--------------------------------------------------

rBinding :: P.ReadM Binding
rBinding = P.eitherReader (A.parseOnly p . T.pack)
  where

  p = pBinding bindingSyntax

--------------------------------------------------

rLicense :: P.ReadM License
rLicense = P.maybeReader parseLicense

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

failP :: String -> ReadM a
failP message = P.ErrorMsg message

{- Notes

  | ErrorMsg        String
  | MissingError    IsCmdStart SomeParser
  | ExpectsArgError String

-}

--------------------------------------------------

{- | 'posixBindingSyntax'.

(a « = » (the equals sign) is more natural than a « : » (colon);
TODO ensure that, on the cmdln, it won't be gobbled under « optparse-applicative »'s syntax)

-}

bindingSyntax :: BindingSyntax
bindingSyntax = posixBindingSyntax

--------------------------------------------------
--------------------------------------------------