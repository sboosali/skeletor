{-# LANGUAGE CPP #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.File.Encoding where

--------------------------------------------------


--------------------------------------------------
--------------------------------------------------

import qualified "filepath"   System.FilePath as File

--------------------------------------------------

import qualified "text" Data.Text      as StrictText
import qualified "text" Data.Text.Lazy as LazyText

import qualified "text" Data.Text.IO      as StrictText
import qualified "text" Data.Text.Lazy.IO as LazyText

import qualified "text" Data.Text.Encoding as Encoding

--------------------------------------------------

import qualified "bytestring"       Data.ByteString      as StrictByteString
import qualified "bytestring"       Data.ByteString.Lazy as LazyByteString

import qualified "bytestring"       Data.ByteString.IO      as StrictByteString
import qualified "bytestring"       Data.ByteString.Lazy.IO as LazyByteString

--------------------------------------------------
--------------------------------------------------

import qualified "base" GHC.IO as GHC

--------------------------------------------------

import Prelude_location

--------------------------------------------------
--------------------------------------------------

--------------------------------------------------
--------------------------------------------------

{-|

@CP-1252@:

> Windows-1252 or CP-1252 (code page – 1252) is a single-byte character encoding of the Latin alphabet, used by default in the legacy components of Microsoft Windows in English and some other Western languages (other languages use different default encodings).
> It is probably the most-used 8-bit character encoding in the world.

-}

data Encoding

 = UTF8_Encoding

 | ASCII_Encoding
 | Latin1_Encoding
 | CP1252_Encoding

 | UTF16_LittleEndian_Encoding
 | UTF16_BigEndian_Encoding
 | UTF32_LittleEndian_Encoding
 | UTF32_BigEndian_Encoding

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultEncoding'@

instance Default Encoding where
  def = defaultEncoding

--------------------------------------------------

-- | @= 'UTF8_Encoding'@

defaultEncoding :: Encoding
defaultEncoding = UTF8_Encoding

--------------------------------------------------
--------------------------------------------------

{-| 'NewlineConversion' represents the character(s)
that represent a newline in a file.

the 'NewlineConversion' type is equivalent to @GHC@'s 'GHC.IO.Newline',
but it has more (i.e. non-@base@) instances.

-}

data NewlineConversion

  = LF_Newline    -- ^ @(['\n'] :: [Char])@ a.k.a. "Line Feed".
  | CRLF_Newline  -- ^ @(['\r', '\n'] :: [Char])@ a.k.a. "Carriage Return, then Line Feed".

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{- | The *native* newline representation for the current platform:

* 'CRLF_Newline' — on Windows systems.
* 'LF_Newline'   — on non-Windows systems;
                   in particular, on UNIX systems.

-}

nativeNewlineConversion :: NewlineConversion

#if defined(mingw32_HOST_OS)
nativeNewlineConversion = CRLF_Newline

#else
nativeNewlineConversion = LF_Newline
#endif

--------------------------------------------------

-- | (Isomorphism: @into@)

toNewlineConversion :: GHC.Newline -> NewlineConversion
toNewlineConversion = \case

  GHC.LF    -> LF_Newline
  GHC.CRLF  -> CRLF_Newline

--------------------------------------------------

-- | (Isomorphism: @from@)

fromNewlineConversion :: NewlineConversion -> GHC.Newline
fromNewlineConversion = \case

  LF_Newline    -> GHC.LF
  CRLF_Newline  -> GHC.CRLF

--------------------------------------------------
--------------------------------------------------
