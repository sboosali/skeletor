--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Haskell.Turtle

  ( module Skeletor.Haskell.Turtle
  ) where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Skeletor.Haskell.Types

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "turtle" Turtle as H -- (MNEMONIC: "H" for "turtle sHell")
import           "turtle" Turtle (Shell, Pattern, Line)

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "containers" Data.Map as Map
-- import           "containers" Data.Map (Map)

--------------------------------------------------

import qualified "text" Data.Text as T
import           "text" Data.Text (Text)

--------------------------------------------------

import qualified "base" Data.List.NonEmpty as NonEmpty

--------------------------------------------------

import           "base" Data.Monoid (Alt(..))

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_skeletor hiding (Text)

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-| Run @sed@ on the given template file with the given substitutions.

-}

-- :: _ -> _
-- = _

sedTemplate :: TemplateBinding -> SrcDst -> IO ()
sedTemplate (TemplateBinding bindings) SrcDst{ input, output } =

  program

  where

  program :: IO ()
  program
    = (dstLines output <$> outputLines)
    & (maybe nothing id)

  outputLines :: Maybe (Shell Line)
  outputLines = do

    if   Map.null bindings
    then Nothing
    else Just (H.sed patterns inputLines)

  inputLines :: Shell Line
  inputLines = srcLines input

  patterns :: Pattern Text
  patterns = getAlt (bindings & Map.foldMapWithKey toPatternAny)

  toPatternAny :: Text -> Text -> Alt Pattern Text
  toPatternAny variable value = Alt (value <$ H.has (H.text variable))

--------------------------------------------------
--------------------------------------------------

srcLines
  :: Src
  -> Shell Line

srcLines = \case

  SrcStdin          -> H.stdin
  SrcFile  filepath -> H.input  (H.fromText (T.pack filepath))  -- NOTE « H.fromText » is partial.
  SrcLines ts       -> H.select (textsToLines ts)

--------------------------------------------------

dstLines
  :: (MonadIO io)
  => Dst
  -> (Shell Line -> io ())

dstLines = \case

  DstStdout          -> H.stdout
  DstFile   filepath -> H.output (H.fromText (T.pack filepath))  -- NOTE « H.fromText » is partial.

--------------------------------------------------
--------------------------------------------------

textsToLines :: [Text] -> [Line]
textsToLines ts = ts & concatMap go

  where

  go :: Text -> [Line]
  go = H.textToLines > NonEmpty.toList

--------------------------------------------------
--------------------------------------------------
