
--------------------------------------------------
--------------------------------------------------

{-| 'Exception' Types.

-}

module Skeletor.Haskell.Errors

  ( SkeletorHaskellSyntaxError(..)
  , SkeletorHaskellTypeError(..)

  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------

import "base" Control.Exception (Exception(..))

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-|

-}

newtype SkeletorHaskellSyntaxError = SkeletorHaskellSyntaxError

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance Exception SkeletorHaskellSyntaxError where

--------------------------------------------------

instance IsString SkeletorHaskellSyntaxError where
  fromString = coerce

--------------------------------------------------
--------------------------------------------------

{-|

-}

newtype SkeletorHaskellTypeError = SkeletorHaskellTypeError

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance Exception SkeletorHaskellTypeError where

--------------------------------------------------

instance IsString SkeletorHaskellTypeError where
  fromString = coerce

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------