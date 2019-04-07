
--------------------------------------------------
--------------------------------------------------

{-| Types for the @Git@ verion-control-system.

This wraps the @gitlib@ C Library.

-}

module Skeletor.Core.Git.Types where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Core.SHA.Types

--------------------------------------------------
--------------------------------------------------

import           "text" Data.Text (Text)
import qualified "text" Data.Text as T
import qualified "text" Data.Text.Encoding as T

--------------------------------------------------

import           "bytestring" Data.ByteString (ByteString)
import qualified "bytestring" Data.ByteString.Base16 as B16

--------------------------------------------------
--------------------------------------------------

data GitException

  -- = BackendError             Text
  -- | GitError                 Text
  -- | RepositoryNotExist
  -- | RepositoryInvalid
  -- | RepositoryCannotAccess   Text
  -- | BlobCreateFailed         Text
  -- | BlobEmptyCreateFailed
  -- | BlobEncodingUnknown      Text
  -- | BlobLookupFailed
  -- | DiffBlobFailed           Text
  -- | DiffPrintToPatchFailed   Text
  -- | DiffTreeToIndexFailed    Text
  -- | IndexAddFailed           TreeFilePath Text
  -- | IndexCreateFailed        Text
  -- | PathEncodingError        Text
  -- | PushNotFastForward       Text
  -- | TagLookupFailed          Text
  -- | TranslationException     Text
  -- | TreeCreateFailed         Text
  -- | TreeBuilderCreateFailed
  -- | TreeBuilderInsertFailed  TreeFilePath
  -- | TreeBuilderRemoveFailed  TreeFilePath
  -- | TreeBuilderWriteFailed   Text
  -- | TreeLookupFailed
  -- | TreeCannotTraverseBlob
  -- | TreeCannotTraverseCommit
  -- | TreeEntryLookupFailed    TreeFilePath
  -- | TreeUpdateFailed
  -- | TreeWalkFailed           Text
  -- | TreeEmptyCreateFailed
  -- | CommitCreateFailed
  -- | CommitLookupFailed       Text
  -- | ReferenceCreateFailed    RefName
  -- | ReferenceDeleteFailed    RefName
  -- | RefCannotCreateFromPartialOid
  -- | ReferenceListingFailed   Text
  -- | ReferenceLookupFailed    RefName
  -- | ObjectLookupFailed       Text Int
  -- | ObjectRefRequiresFullOid
  -- | OidCopyFailed
  -- | OidParseFailed           Text
  -- | QuotaHardLimitExceeded   Int Int

  deriving (Eq, Show, Typeable)

--------------------------------------------------
--------------------------------------------------
