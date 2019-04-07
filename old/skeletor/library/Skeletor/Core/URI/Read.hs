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

wraps 'HTTP.httpLbs'.

-}

readURI
  :: (MonadIO m, MonadThrow m)
  => RemoteFileLocation -> m (Text)

readURI (uri) = do

  request <- HTTP.parseRequest (T.pack uri)

  manager <- do
          if   HTTP.secure request
          then HTTP.newTlsManager
          else HTTP.newManager HTTP.defaultManagerSettings

  response <- HTTP.httpLbs (request { HTTP.method = HTTP.methodGet }) manager

  let status = HTTP.responseStatus response

  let headers = HTTP.responseHeaders response

  handleStatus response headers status

  where

  handleStatus response headers status

    | status == HTTP.ok200 = do

        let bytestring = HTTP.responseBody response
        let body = handleBody headers bytestring
        return body

    | status == HTTP.created201 = do

        let location = HTTP.hLocation
        return ""

    | status == HTTP.found302 = do

        return ""

    | otherwise = return ""

  handleBody headers bytestring = _

  lookupHeader :: ResponseHeaders -> HeaderName -> Maybe ByteString
  lookupHeader = List.lookup

--------------------------------------------------
--------------------------------------------------

{-| Download (a.k.a. "clone") the (given) Git repository;
into a directory (of files).

-}

readGitRepository :: GitLocation -> IO (_)
readGitRepository = _

--------------------------------------------------
--------------------------------------------------
