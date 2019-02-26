{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}

--------------------------------------------------

{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Parsers

  ( rProject

  --, rLocation
  , rFetchBy
  , rURI

  , rLicense
  , rOSILicense
  , rFLOSSLicense

  ) where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Types

--import Program.Skeletor.Haskell.Options
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

import qualified "megaparsec" Text.Megaparsec       as Megaparsec
import qualified "megaparsec" Text.Megaparsec.Error as Megaparsec

--------------------------------------------------

import qualified "modern-uri" Text.URI as URI
import           "modern-uri" Text.URI (URI)

--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative      as Optparse
import qualified "optparse-applicative" Options.Applicative.Help as Optparse hiding (fullDesc)
import           "optparse-applicative" Options.Applicative      (ReadM)

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "text" Data.Text as T
import           "text" Data.Text (Text)

-- NOTE « attoparsec » uses strict « Text ».

--------------------------------------------------

import           "base" Control.Exception (Exception(..), SomeException(..))
import           "base" Data.Maybe
import           "base" Data.Semigroup

--------------------------------------------------

import Prelude_exe hiding (Text)

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

--------------------------------------------------
-- Parsers ---------------------------------------
--------------------------------------------------

rURI :: Optparse.ReadM URI
rURI = fromMonadParsec (Just "URI") URI.parser

--------------------------------------------------

rProject :: Optparse.ReadM KnownProjectName
rProject = fromMonadThrow parseBuiltinProjectName

--------------------------------------------------
--------------------------------------------------

-- rLocation :: Optparse.ReadM Location
-- rLocation = _

--------------------------------------------------

rFetchBy :: Optparse.ReadM FetchBy
rFetchBy = fromMonadThrow parseFetchBy

--------------------------------------------------
--------------------------------------------------

rLicense :: Optparse.ReadM License
rLicense = fromMonadThrow parseLicense

--------------------------------------------------

rOSILicense :: Optparse.ReadM License
rOSILicense = fromMonadThrow parseOSILicense

--------------------------------------------------

rFLOSSLicense :: Optparse.ReadM License
rFLOSSLicense = fromMonadThrow parseFLOSSLicense

--------------------------------------------------
--------------------------------------------------

rBindings :: Optparse.ReadM Bindings
rBindings = Optparse.eitherReader (Attoparsec.parseOnly p . T.pack)
  where

  p = pBindings bindingSyntax

--------------------------------------------------

rBinding :: Optparse.ReadM Binding
rBinding = Optparse.eitherReader (Attoparsec.parseOnly p . T.pack)
  where

  p = pBinding bindingSyntax

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

attoparsecReader :: Attoparsec.Parser a -> ReadM a
attoparsecReader p = eitherReader (Attoparsec.parseOnly p . T.pack)

-}

--------------------------------------------------

fromMonadThrow
  :: forall a.

    (forall m. MonadThrow m => String -> m a)

  -> ReadM a

fromMonadThrow parse = toReadM parse
  where

  toReadM :: (String -> Either SomeException a) -> ReadM a
  toReadM p = Optparse.eitherReader (p > bimap displayException id)

--------------------------------------------------

fromMonadParsec
  :: forall a.

    Maybe String
  -> (forall e m. Megaparsec.MonadParsec e Text m => m a)

  -> ReadM a

fromMonadParsec name p = fromMonadThrow go
  where

  go :: (MonadThrow m) => String -> m a
  go s = toMonadThrow (Megaparsec.runParser q name' t)
      where

      t = T.pack s

      toMonadThrow e = case e of
          Left  b -> throwM (URI.ParseException b)   --TODO-- issue on megaparsec for a ParseException
          Right x -> return x

  q :: Megaparsec.Parsec Void Text a
  q = p  <* Megaparsec.eof

  name' :: String
  name' = fromMaybe "" name

{-# INLINEABLE fromMonadParsec #-}

--------------------------------------------------

failP :: String -> ReadM a
failP message = Optparse.readerAbort error
  where
  error = Optparse.ErrorMsg message

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