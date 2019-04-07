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

-}

module Skeletor.Core.URI.Types where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import           "modern-uri" Text.URI (URI)
import qualified "modern-uri" Text.URI as URI

--------------------------------------------------

import qualified "unordered-containers" Data.HashMap.Lazy as HashMap
import           "unordered-containers" Data.HashMap.Lazy (HashMap)

--------------------------------------------------
-- Imports ---------------------------------------
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

--import qualified "base" System.IO as IO

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

{-| 

-}

data RemoteFileLocation = RemoteFileLocation

  { remoteLocation :: URI
  }

--------------------------------------------------

{-| A filetree within a Git repository is identified by:

* the location of that repository
  (@URL@s can be both remote and, wtih the @file://@ scheme, local).
* a reference, to @git checkout@.
* a subdirectory (within that reference), to @cd@ into.

-}

data GitLocation = GitLocation

  { gitLocation     :: URI
  , gitReference    :: GitReference
  , gitSubDirectory :: FilePath
  }

--------------------------------------------------
--------------------------------------------------

{-| a Git reference can be:

* a (@SHA256@) commit — the most general reference.
* a tag — created by @git tag@.
* a branch — created by @git checkout --branch@.
* nothing — which defaults to the @HEAD@ commit of the @master@ branch.

See 'normalizeGitReference'.

-}

data GitReference

  = GitCommit Text
  | GitTag    Text
  | GitBranch Text
  | GitNoReference

--------------------------------------------------

-- | injects via 'GitCommit'.

instance IsString GitReference where

  fromString = (GitCommit . fromString)

--------------------------------------------------

-- | @≡ 'GitNoReference'@.

instance Default GitReference where

  def = GitNoReference

--------------------------------------------------
--------------------------------------------------

{-| Normalize any 'GitReference' to a 'GitCommit'.

Each 'GitReference' is equivalent to a single 'GitCommit'
(for a particular snapshot of the repository):

* @'GitTag' "..."@ is (?) equivalent to @'GitCommit' "refs/tags/..."@.
* @'GitBranch' "..."@ is (?) equivalent to @'GitCommit' "refs/heads/..."@.
* @'GitNoReference'@ is (?) equivalent to @'GitCommit' "refs/heads/master"@.

-}

normalizeGitReference :: GitReference -> GitReference
normalizeGitReference = \case

  GitCommit t    -> GitCommit t
  GitTag    t    -> GitCommit ("refs/tags/"  <> t)
  GitBranch t    -> GitCommit ("refs/heads/" <> t)
  GitNoReference -> GitCommit "refs/heads/master"

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code



{-| a _ is an _ file.

-}

type _ = File 





-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------