--------------------------------------------------
--------------------------------------------------

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}

--------------------------------------------------

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------
--------------------------------------------------

{-|

Currently, these URI Schemes are recognized:

* @file://@
* @https://@ (and @http://@)
* @git://@
* @ssh://@

See 'interpretURI'.

-}

module Skeletor.Core.URI.Schemes where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import           "modern-uri" Text.URI (URI(..))
import qualified "modern-uri" Text.URI as URI

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "text"       Data.Text    as T
import qualified "text"       Data.Text.IO as T

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-

{-| Interpret a URI as a Location given its Scheme and\/or its Path.

If the @uri@ has a URI Scheme, assume that.
Otherwise, take the URI Path, and guess the Scheme from the Path.

-}

interpretURI :: URI -> _
interpretURI (URI{}) = _

-}

--------------------------------------------------
--------------------------------------------------

-- | 

fileScheme :: URI.RText URI.Scheme
fileScheme = URI.mkScheme "file" & fromJust

--------------------------------------------------

-- | 

gitScheme :: URI.RText URI.Scheme
gitScheme = URI.mkScheme "git" & fromJust

--------------------------------------------------

-- | 

httpsScheme :: URI.RText URI.Scheme
httpsScheme = URI.mkScheme "https" & fromJust

--------------------------------------------------

-- | 

httpScheme :: URI.RText URI.Scheme
httpScheme = URI.mkScheme "http" & fromJust

--------------------------------------------------

-- | 

sshScheme :: URI.RText URI.Scheme
sshScheme = URI.mkScheme "ssh" & fromJust

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------