{-# LANGUAGE BinaryLiterals #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.URI.Read

  ( readRemote
  , readGit
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------

import Skeletor.Core.URI.Types

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "network-uri" Network.URI as Network

--------------------------------------------------

import qualified "modern-uri" Text.URI as Modern
import           "modern-uri" Text.URI ( URI(..), Authority(..), UserInfo(..), QueryParam(..) )

--------------------------------------------------

import qualified "http-types"      Network.HTTP.Types      as HTTP
import qualified "http-client"     Network.HTTP.Client     as HTTP hiding (path)
import qualified "http-client-tls" Network.HTTP.Client.TLS as HTTP

--------------------------------------------------

import qualified "unordered-containers" Data.HashMap.Lazy as HashMap
import           "unordered-containers" Data.HashMap.Lazy (HashMap)

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "text"       Data.Text    as T
import qualified "text"       Data.Text.IO as T

--------------------------------------------------

import qualified "bytestring" Data.ByteString as B
import           "bytestring" Data.ByteString (ByteString)

--------------------------------------------------

import qualified "base" Data.List as List

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-| Download the (given) URI (a.k.a. URL);
into a file.

wraps 'HTTP.httpLbs'.

-}

readURI
  :: (MonadIO m, MonadThrow m)
  => RemoteFileLocation -> m (Text)

readURI RemoteFileLocation { remoteLocation } = do

  request <- HTTP.parseRequest remoteLocation

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
        let body       = handleBody headers bytestring
        return body

    | status == HTTP.created201 = do

        let location = HTTP.hLocation
        return ""

    | status == HTTP.found302 = do

        return ""

    | otherwise = return ""

  handleBody headers bytestring = _

  lookupHeader :: HTTP.ResponseHeaders -> HTTP.HeaderName -> Maybe ByteString
  lookupHeader = List.lookup

--------------------------------------------------

{-| Download (a.k.a. "clone") the (given) Git repository;
into a directory (of files).

-}

readGit :: GitLocation -> IO (_)
readGit = _

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

convertURL :: Modern.URI -> Network.URI
convertURL modernURI@URI{..} = networkURI
  where
  ------------------------------

  networkURI :: Network.URI
  networkURI = Network.URI

    { Network.uriScheme     = uriScheme & maybe "" get
    , Network.uriAuthority  = Just networkAuth
    , Network.uriPath       = ""
    , Network.uriQuery      = uriQuery    & maybe "" (get > ("?" ++))
    , Network.uriFragment   = uriFragment & maybe "" (get > ("#" ++))
    }

  networkAuth :: Network.URIAuth
  networkAuth = Network.URIAuth

    { Network.uriUserInfo = ""
    , Network.uriRegName  = ""
    , Network.uriPort     = ""
    }

  ------------------------------

  Authority { authUserInfo, authHost, authPort } = uriAuthority

  ------------------------------

  UserInfo { uiUsername, uiPassword } = userInfo

  ------------------------------

  get :: Modern.RText l -> Text
  get = Modern.unRText

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

--------------------------------------------------
-- network-uri

data URI = URI

    { uriScheme     :: String           -- ^ @foo:@
    , uriAuthority  :: Maybe URIAuth    -- ^ @\/\/anonymous\@www.haskell.org:42@
    , uriPath       :: String           -- ^ @\/ghc@
    , uriQuery      :: String           -- ^ @?query@
    , uriFragment   :: String           -- ^ @#frag@
    }

--------------------------------

data URIAuth = URIAuth

    { uriUserInfo   :: String           -- ^ @anonymous\@@
    , uriRegName    :: String           -- ^ @www.haskell.org@
    , uriPort       :: String           -- ^ @:42@
    }

--------------------------------------------------
-- modern-uri

data URI = URI

  { uriScheme :: Maybe (RText 'Scheme)
    -- ^ URI scheme, if 'Nothing', then the URI reference is relative
  , uriAuthority :: Either Bool Authority
    -- ^ 'Authority' component in 'Right' or a 'Bool' value in 'Left'
    -- indicating if 'uriPath' path is absolute ('True') or relative
    -- ('False'); if we have an 'Authority' component, then the path is
    -- necessarily absolute, see 'isPathAbsolute'
    --
    -- __Note__: before version /0.1.0.0/ type of 'uriAuthority' was
    -- @'Maybe' 'Authority'@
  , uriPath :: Maybe (Bool, NonEmpty (RText 'PathPiece))
    -- ^ 'Nothing' represents the empty path, while 'Just' contains an
    -- indication 'Bool' whether the path component has a trailing slash,
    -- and the collection of path pieces @'NonEmpty' ('RText' 'PathPiece')@.
    --
    -- __Note__: before version /0.2.0.0/ type of 'uriPath' was @['RText'
    -- 'PathPiece']@.
  , uriQuery :: [QueryParam]
    -- ^ Query parameters, RFC 3986 does not define the inner organization
    -- of query string, so we deconstruct it following RFC 1866 here
  , uriFragment :: Maybe (RText 'Fragment)
    -- ^ Fragment, without @#@
  }

--------------------------------

data Authority = Authority

  { authUserInfo :: Maybe UserInfo
    -- ^ User information
  , authHost :: RText 'Host
    -- ^ Host
  , authPort :: Maybe Word
    -- ^ Port number
  }

--------------------------------

data UserInfo = UserInfo

  { uiUsername :: RText 'Username
    -- ^ Username
  , uiPassword :: Maybe (RText 'Password)
    -- ^ Password, 'Nothing' means that there was no @:@ character in the
    -- user info string
  }

--------------------------------

data QueryParam

  = QueryFlag (RText 'QueryKey)
    -- ^ Flag parameter

  | QueryParam (RText 'QueryKey) (RText 'QueryValue)
    -- ^ Key–value pair

--------------------------------

data RTextLabel

  = Scheme             -- ^ See 'mkScheme'
  | Host               -- ^ See 'mkHost'
  | Username           -- ^ See 'mkUsername'
  | Password           -- ^ See 'mkPassword'
  | PathPiece          -- ^ See 'mkPathPiece'
  | QueryKey           -- ^ See 'mkQueryKey'
  | QueryValue         -- ^ See 'mkQueryValue'
  | Fragment           -- ^ See 'mkFragment'

--------------------------------

class RLabel (l :: RTextLabel) where

  rcheck     :: Proxy l -> Text -> Bool
  rnormalize :: Proxy l -> Text -> Text
  rlabel     :: Proxy l         -> RTextLabel

--------------------------------

unRText :: RText l -> Text

--------------------------------------------------
--

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------