
--------------------------------------------------
--------------------------------------------------

{-| @SHA@ bytestrings (e.g. @SHA-256@).

(Re-export all types and functions of this module's submodules.)

-}

module Skeletor.Core.SHA
  (
    -- * (Types and instances.)
    module Skeletor.Core.SHA.Types

    -- * Refining\/redering 'SHA's.
  , module Skeletor.Core.SHA
  ) where

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

shaToText :: SHA -> Text
shaToText (SHA bs) = T.decodeUtf8 (B16.encode bs)

--------------------------------------------------

textToSha :: Monad m => Text -> m SHA
textToSha t =
    case B16.decode $ T.encodeUtf8 t of
        (bs, "") -> return (SHA bs)
        _ -> fail "Invalid base16 encoding"

--------------------------------------------------
--------------------------------------------------
