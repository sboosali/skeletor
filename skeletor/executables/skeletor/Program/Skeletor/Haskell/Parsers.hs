{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Parsers

  ( pURI
  , pFetchBy
  , pLicense
  , pLocation
  , pProject

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
import Skeletor.Haskell.License
import Skeletor.Haskell.Variable.Binding

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "attoparsec" Data.Attoparsec.Text as Attoparsec

--------------------------------------------------

import qualified "megaparsec" Text.Megaparsec as Megaparsec

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
import           "optparse-applicative" Options.Applicative      (ReadM)

--------------------------------------------------

import           "base" Data.Maybe
import           "base" Data.Semigroup

--------------------------------------------------

import Prelude_exe

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------



--------------------------------------------------
-- Parsers ---------------------------------------
--------------------------------------------------

pURI :: Optparse.ReadM URI
pURI = _

--------------------------------------------------

pProject :: Optparse.ReadM Project
pProject = _

--------------------------------------------------

pLicense :: Optparse.ReadM License
pLicense = _

--------------------------------------------------

pLocation :: Optparse.ReadM Location
pLocation = _

--------------------------------------------------

pFetchBy :: Optparse.ReadM FetchBy
pFetchBy = _

--------------------------------------------------

rBindings :: Optparse.ReadM Bindings
rBindings = Optparse.eitherReader (A.parseOnly p . T.pack)
  where

  p = pBindings bindingSyntax

--------------------------------------------------

rBinding :: Optparse.ReadM Binding
rBinding = Optparse.eitherReader (A.parseOnly p . T.pack)
  where

  p = pBinding bindingSyntax

--------------------------------------------------

rLicense :: Optparse.ReadM License
rLicense = Optparse.maybeReader parseSpdxLicenseIdentifier

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

-- fromMonadThrow :: forall a. (forall m. MonadThrow m => Text -> m a) -> ReadM a

-- fromMonadParsec :: forall a. (forall e m. MonadParsec e Text m => m a) -> ReadM a

{- Note:

we don't expose « ParseException » from the « megaparsec » package (to the user).
instead, we render it, and expose only the « String » (or « Doc »).

eitherReader :: (String -> Either String a) -> ReadM a


import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

attoparsecReader :: A.Parser a -> ReadM a
attoparsecReader p = eitherReader (A.parseOnly p . T.pack)

-}

--------------------------------------------------

fromMonadThrow
  :: forall a. ()

  => Maybe String
  -> (forall m. MonadThrow m => Text -> m a)

  -> ReadM a

fromMonadThrow _name parse = toReadM parse

  toReadM p = Optparse.eitherReader p

--------------------------------------------------

fromMonadParsec
  :: forall a. ()

  => Maybe String
  -> (forall e m. MonadParsec e Text m => m a)

  -> ReadM a

fromMonadParsec name p = fromMonadThrow name go
  where

  go :: (MonadThrow m) => Text -> m a
  go = toMonadThrow (Megaparsec.runParser q name')
      where

      toMonadThrow e = case e of
          Left  b -> throwM (ParseException b)
          Right x -> return x

  q :: Parsec Void Text a
  q = p  <* eof

  name' :: String
  name' = fromMaybe "" name

{-# INLINEABLE fromMonadParsec #-}

--------------------------------------------------

failP :: String -> ReadM a
failP message = Optparse.ErrorMsg message

{- Notes

Optparse.ParseError:

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