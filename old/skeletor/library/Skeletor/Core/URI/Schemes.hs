{-# LANGUAGE BinaryLiterals #-}

--------------------------------------------------
--------------------------------------------------

{-|

Currently, these URI Schemes are recognized:

* @file://@
* @https://@ (and @http://@)
* @git://@
* @ssh://@
* @://@
* @://@

See 'interpretURI'.

-}

module Skeletor.Core.URI.Schemes where

--------------------------------------------------
--------------------------------------------------


--------------------------------------------------
--------------------------------------------------

import           "modern-uri" Text.URI (URI(..))
import qualified "modern-uri" Text.URI as URI

--------------------------------------------------

import qualified "text"       Data.Text    as T
import qualified "text"       Data.Text.IO as T

--------------------------------------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------
--------------------------------------------------

{-| Interpret a URI as a Location given its Scheme and\/or its Path.

If the @uri@ has a URI Scheme, assume that.
Otherwise, take the URI Path, and guess the Scheme from the Path.

-}

interpretURI :: URI -> _
interpretURI (URI uri) = _

--------------------------------------------------
--------------------------------------------------

-- | 

fileScheme :: URI.RText URI.Scheme
fileScheme = mkScheme "file" & fromJust

--------------------------------------------------

-- | 

gitScheme :: URI.RText URI.Scheme
gitScheme = mkScheme "git" & fromJust

--------------------------------------------------

-- | 

httpsScheme :: URI.RText URI.Scheme
httpsScheme = mkScheme "https" & fromJust

--------------------------------------------------

-- | 

httpScheme :: URI.RText URI.Scheme
httpScheme = mkScheme "http" & fromJust

--------------------------------------------------

-- | 

sshScheme :: URI.RText URI.Scheme
sshScheme = mkScheme "ssh" & fromJust

--------------------------------------------------
--------------------------------------------------
