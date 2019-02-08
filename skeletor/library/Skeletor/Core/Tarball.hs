--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.Tarball where

--------------------------------------------------
--------------------------------------------------

import qualified "tar" Codec.Archive.Tar as Tar

--------------------------------------------------

import qualified "zlib" Codec.Compression.GZip as GZip
import qualified "zlib" Codec.Compression.Zlib as Zlib

--------------------------------------------------

import qualified "bytestring" Data.ByteString.Lazy as ByteString

--------------------------------------------------
--------------------------------------------------

readTarball :: _
readTarball dir tar = _

--------------------------------------------------

readTarballGZip :: _
readTarballGZip dir tar = Tar.unpack dir . Tar.read . GZip.decompress =<< BS.readFile tar


--------------------------------------------------

readTarballZlib :: _
readTarballZlib dir tar = Tar.unpack dir . Tar.read . Zlib.decompress =<< BS.readFile tar

--------------------------------------------------
--------------------------------------------------
