{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ApplicativeDo #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Haskell.Variable.Binding

  ( module Skeletor.Haskell.Variable.Binding.Types
  , module Skeletor.Haskell.Variable.Binding
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import Skeletor.Haskell.Variable.Binding.Types

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

-- import 

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "attoparsec" Data.Attoparsec.Text as P
import           "attoparsec" Data.Attoparsec.Text (Parser)

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

--import qualified "text" Data.Text as T
import           "text" Data.Text (Text)

-- NOTE « attoparsec » uses strict « Text ».

--------------------------------------------------

import qualified "base" Data.Char as Char

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_skeletor hiding (Text)

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-|

>>> parseBindings posixBindingSyntax "abc=123;xyz=xyz"
Right (Bindings [Binding {name = "abc", value = "123"}, Binding {name = "xyz", value = "xyz"}])

-}

parseBindings :: BindingSyntax -> Text -> Either String Bindings
parseBindings syntax = P.parseOnly pBindingsEOF
  where

  pBindingsEOF = (pBindings syntax <* P.endOfInput)

--------------------------------------------------
--------------------------------------------------

{-|

>>> parseBinding def "abc=xyz123"
Right (Binding {name = "abc", value = "xyz123"})

-}

parseBinding :: BindingSyntax -> Text -> Either String Binding
parseBinding syntax = P.parseOnly pBindingEOF
  where

  pBindingEOF = (pBinding syntax <* P.endOfInput)

--------------------------------------------------
--------------------------------------------------

{-|

-}

pBindings :: BindingSyntax -> Parser Bindings
pBindings syntax@BindingSyntax{..} = do

  optional pSeparator
  bs <- pBindings'
  optional pSeparator

  return (Bindings bs)

  where

  pBindings' = pBinding syntax `P.sepBy` pSeparator

  pSeparator = P.char separator $> ()

--------------------------------------------------
--------------------------------------------------

{-|

-}

pBinding :: BindingSyntax -> Parser Binding
pBinding BindingSyntax{..} = do

  name  <- pName
  pOperator
  value <- pValue

  return Binding{..}

  where

    pName      = P.takeWhile1 (isNameChar)
    pValue     = P.takeWhile1 (isValueChar)

    pOperator = P.char operator $> ()

    isNameChar c      = Char.isAlpha c    && (not (isOperatorChar c || Char.isSpace c))
    isValueChar c     = Char.isAlphaNum c || Char.isPrint c

    isOperatorChar c = (operator == c)

    --NOTE
    --
    -- λ> isAlphaNum ' '
    --    False

--------------------------------------------------
--------------------------------------------------
