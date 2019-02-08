{-# LANGUAGE BinaryLiterals #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.URI.Read where

--------------------------------------------------

import Skeletor.Core.URI.Types

--------------------------------------------------
--------------------------------------------------

import           "network-uri" Network.URI (URI)
import qualified "network-uri" Network.URI as URI

--------------------------------------------------

-- import           "modern-uri" Text.URI (URI)
-- import qualified "modern-uri" Text.URI as URI

--------------------------------------------------
--------------------------------------------------

import qualified "unordered-containers" Data.HashMap.Lazy as HashMap
import           "unordered-containers" Data.HashMap.Lazy (HashMap)

--------------------------------------------------

import qualified "text"       Data.Text    as T
import qualified "text"       Data.Text.IO as T

--------------------------------------------------

import qualified "bytestring" Data.ByteString as B
import           "bytestring" Data.ByteString (ByteString)

--------------------------------------------------

import Prelude_location

--------------------------------------------------
--------------------------------------------------

{-| Download the (given) URI (a.k.a. URL);
into a file.

-}

readURIGET :: RemoteFileLocation -> IO (_)
readURIGET = _

--------------------------------------------------

{-| Download (a.k.a. "clone") the (given) Git repository;
into a directory (of files).

-}

readGitRepository :: GitLocation -> IO (_)
readGitRepository = _

--------------------------------------------------
--------------------------------------------------
