{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE BinaryLiterals #-}

--------------------------------------------------
--------------------------------------------------

{-| Locations of templates.

(Re-export all types and functions of this module's submodules.)

-}

module Skeletor.Haskell.Location
  (
    -- * (Types and instances.)
    module Skeletor.Haskell.Location.Types

  , readLocation
  ) where

--------------------------------------------------

import Skeletor.Haskell.Location.Types

--------------------------------------------------
--------------------------------------------------

import           "modern-uri" Text.URI (URI)
import qualified "modern-uri" Text.URI as URI

--------------------------------------------------

import qualified "http-types"      Network.HTTP.Types
import qualified "http-client"     Network.HTTP.Client hiding (path)
import qualified "http-client-tls" Network.HTTP.Client.TLS

--------------------------------------------------

import qualified "unordered-containers" Data.HashMap.Lazy as HashMap
import           "unordered-containers" Data.HashMap.Lazy (HashMap)

--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import qualified "filepath"   System.FilePath as File

--------------------------------------------------

import qualified "text"       Data.Text    as T
import qualified "text"       Data.Text.IO as T

--------------------------------------------------

import qualified "bytestring" Data.ByteString as B

--import qualified "bytestring" Data.ByteString.Lazy as B

--------------------------------------------------

import qualified "base" System.Environment as IO
--import qualified "base" System.IO as IO

import qualified "base" Data.List as List

--------------------------------------------------

import Prelude_location

--------------------------------------------------
--------------------------------------------------

{-|

-}

--------------------------------------------------
--------------------------------------------------

{-|

-}

parseLocation :: Location -> Either LocationParseError URI
parseLocation = _

--------------------------------------------------
--------------------------------------------------

{-|

A 'Location' can be identified by:

* itself           — i.e. inline contents (into a string or tree).
* a filepath       — a directory (the root of the files being located).
* an archive       — a @.tar@; to be un-archived (into the above).
* a tarball        — a @.tar.gz@; to be decompressed (into the above).
* a URI            — download it, following links ([TODO]: detect cycles,
                     ditto with hardlinks of filepaths).
* a Git repo       — clone the repository.
* a name           — a known name (e.g. built-into your system).
* an env-var       — TODO read the environment variable
                     (containing one of the kinds of locations above).

-}

readLocation :: (MonadIO m) => Location -> m (Located Text)
readLocation = \case

  LocationInlineFile      x -> readInlineFile x
  LocationFile            x -> readFile x

  LocationInlineDirectory x -> readInlineDirectory x
  LocationDirectory       x -> readDirectory x

  LocationURL             x -> readURL x
  LocationGit             x -> readGit x

  LocationArchive         x -> readArchive x
  LocationTarball         x -> readTarball x

  LocationEnvironment     x -> readEnvironmentVariable x

--------------------------------------------------
--------------------------------------------------

-- | wraps 'Data.Text.IO.readFile'.

readFilePath :: (MonadIO m) => FilePath -> m (Text)
readFilePath path = do

  contents <- T.readFile path

  return $ contents

--------------------------------------------------
--------------------------------------------------

-- | wraps ''.

readDirectoryPath :: (MonadIO m) => FilePath -> m (FileTree)
readDirectoryPath path = do

  

  return $ ""

--------------------------------------------------
--------------------------------------------------

-- | wraps ''.

readURI :: (MonadIO m, MonadThrow m) => URI -> m (Text)
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

-- | wraps ''.

read :: (MonadIO m) =>  -> m (Text)
read () = do

  

  return $ ""

--------------------------------------------------
--------------------------------------------------

-- | wraps ''.

read :: (MonadIO m) =>  -> m (Text)
read () = do

  

  return $ ""

--------------------------------------------------
--------------------------------------------------

-- | wraps 'System.Environment.lookupEnv'.

readEnvironmentVariable :: (MonadIO m) => EV -> m (Text)
readEnvironmentVariable (EV k) = do

  v <- IO.lookupEnv (T.unpack k) <&> go

  return $ v

  where
  go = maybe "" id > T.pack

--------------------------------------------------
--------------------------------------------------

{- Notes / Old Code



HTTP.hContentType





read :: (MonadIO m) =>  -> m (Text)
read () = do

  

  return $ ""




The HTTP 201 Created success status response code indicates that the request has succeeded and has led to the creation of a resource. The new resource is effectively created before this response is sent back and the new resource is returned in the body of the message, its location being either the URL of the request, or the content of the Location header.

The common use case of this status code is as the result of a POST request.





Client request:

GET /index.html HTTP/1.1
Host: www.example.com

Server response:

HTTP/1.1 302 Found
Location: http://www.iana.org/domains/example/




httpLbs :: Request -> Manager -> IO (Response ByteString)

responseHeaders :: Response body -> ResponseHeaders

responseBody :: Response body -> body

type Header = (HeaderName, ByteString) Source#

Header

type HeaderName = CI ByteString Source#

Header name

type RequestHeaders = [Header] Source#

Request Headers

type ResponseHeaders = [Header] Source#

Response Headers




Cookie	 
cookie_name :: ByteString	 
cookie_value :: ByteString	 
cookie_expiry_time :: UTCTime	 
cookie_domain :: ByteString	 
cookie_path :: ByteString	 
cookie_creation_time :: UTCTime	 
cookie_last_access_time :: UTCTime	 
cookie_persistent :: Bool	 
cookie_host_only :: Bool	 
cookie_secure_only :: Bool	 
cookie_http_only :: Bool



data URI Source

Represents a general universal resource identifier using its component parts.

For example, for the URI

  foo://anonymous@www.haskell.org:42/ghc?query#frag
the components are:

Constructors

URI	 
uriScheme :: String
 foo:
uriAuthority :: Maybe URIAuth
 //anonymous@www.haskell.org:42
uriPath :: String
 /ghc
uriQuery :: String
 ?query
uriFragment :: String
 #frag





-}
--------------------------------------------------