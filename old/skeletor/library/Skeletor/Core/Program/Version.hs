-------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.Program.Version

  ( toVersion

  ) where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

--import Skeletor.Core.Program.Types

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "trifecta" Text.Trifecta as P
import           "trifecta" Text.Trifecta (Parser)

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

--import           "base" Control.Applicative

--------------------------------------------------

import qualified "base" Data.Version as Version
import           "base" Data.Version (Version)

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

data ParseVersionConfig = ParseVersionConfig

  { minimumNumberOfVersions :: Maybe Natural
  , maximumNumberOfVersions :: Maybe Natural
  }

  deriving (Eq,Show)

--------------------------------------------------

defaultParseVersionConfig :: ParseVersionConfig
defaultParseVersionConfig = ParseVersionConfig{..}
  where

  minimumNumberOfVersions = Just 2
  maximumNumberOfVersions = Nothing

--------------------------------------------------

checkParseVersionConfig :: ParseVersionConfig -> Bool
checkParseVersionConfig ParseVersionConfig{..}

  = (minimumNumberOfVersions >= 1)
 && (maximumNumberOfVersions >= 1))
 && (minimumNumberOfVersions <= maximumNumberOfVersions)

--------------------------------------------------
--------------------------------------------------

{-| Parse a version from the (human-readable, non-standard) output of
invoking a program with @--version@.

Examples for which this parsing works:

@
$ git --version
git version 2.19.2

$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.6.3

$ ghc --numeric-version
8.6.3
@

-}

toVersion :: [String] -> Maybe Version
toVersion lines =

  parse

  where

  parse = P.parseString (pVersion defaultParseVersionConfig) mempty

--------------------------------------------------
--------------------------------------------------

-- | Parse a whole line that may have a version within it.

pVersionLine :: ParseVersionConfig -> Parser Version
pVersionLine config = do

  P.skipMany pNotVersionChar
  pVersionToken
  P.skipMany pNotVersionChar

  where

  pNotVersionChar = P.noneOfSet csVersion

  csVersion = ".0123456789"

  pVersionToken = pVersion config

--------------------------------------------------

{- | Parse a version.

a 'Version' is: one-or-more dot-separated numbers.

This ignores (i.e. it neither fails on nor picks up)
trailing alphanumerics.

e.g.:

@
>>> (P.parseString pVersion mempty) "1.2.3.4prealpha"
Version {versionBranch = [1,2,3,4], versionTags = []}
@

-}

pVersion :: ParseVersionConfig -> Parser Version
pVersion config@ParseVersionConfig{..} = do

  if   checkParseVersionConfig config
  then go <$> p
  else P.unexpected (show config)

  where

  p = pNumber *> pDotNumber

  pDotNumber = pDot *> pNumber

  pDot = P.char '.'

  pNumber = P.digit
    parseAdd = parens $ do

      P.count minimumNumberOfVersions maximumNumberOfVersions

        x <- parseExpr
        _ <- symbolic '+'
        y <- parseExpr
        pure (Add x y)

    parseLit = Lit <$> integer

  go :: [String] -> Version
  go = Version{versionBranch, versionTags = []}
      where
      versionBranch = 

--------------------------------------------------
--------------------------------------------------
