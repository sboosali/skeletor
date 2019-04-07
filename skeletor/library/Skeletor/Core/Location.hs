--------------------------------------------------
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}

--------------------------------------------------
--------------------------------------------------

{-| Locations of templates.

(Re-export all types and functions of this module's submodules.)

-}

module Skeletor.Core.Location
  (
    -- * (Types and instances.)
    module Skeletor.Core.Location.Types

  , readLocation
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------

import Skeletor.Core.Location.Types
-- import Skeletor.Core.Archive
-- import Skeletor.Core.Tarball

--------------------------------------------------
-- Imports ---------------------------------------
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
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "filepath"   System.FilePath as File

--------------------------------------------------

import qualified "text"       Data.Text    as T
import qualified "text"       Data.Text.IO as T

--------------------------------------------------

import qualified "bytestring" Data.ByteString as B

--------------------------------------------------

import qualified "base" Control.Exception as E

import qualified "base" System.Environment as IO
--import qualified "base" System.IO as IO

import qualified "base" Data.List as List

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-|

A 'Location' can be identified by:

* itself           — i.e. inline contents (into a string or tree).
* a filepath       — a directory (the root of the files being located).
* an archive       — a @.tar@ file; to be un-archived (into the above).
* a tarball        — a @.tar.gz@ file; to be decompressed (into the above).
* a URI            — a link; to be downloaded, following links ([TODO]: detect cycles,
                     ditto with hardlinks of filepaths).
* a Git repo       — clone the repository.
* a name           — a known name (e.g. built-into your @skeletor-*@ plugin, or registered via a "Skeletor.Core.Repository").

-}

readLocation :: (MonadIO m) => Location -> m (Located Text)
readLocation = \case

  LocationDirectoryPath             x -> readPath x

  -- LocationDirectoryURL             x -> readURI x
  -- LocationDirectoryGit             x -> readGit x

  -- LocationDirectoryArchive         x -> readArchive x
  -- LocationDirectoryTarball         x -> readTarball x

--------------------------------------------------
--------------------------------------------------

-- | wraps 'Data.Text.IO.readFile'.

readPath :: (MonadIO m) => FilePath -> m (Located Text)
readPath path = io do

  reader `E.catch` handler

  where

  reader = do

    contents <- T.readFile path
    return (YesLocated contents)

  handler :: E.IOException -> IO (Located Text)
  handler e = do

    let s = (show e)
    return (NotLocated s)

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

{- Notes / Old Code...



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
-- EOF -------------------------------------------
--------------------------------------------------