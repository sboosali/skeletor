{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DataKinds             #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Core.Types where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

--import Skeletor.Core.Types

--import Skeletor.Haskell.Types

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "generic-lens" Data.Generics.Product as G

--------------------------------------------------

import           "modern-uri" Text.URI (URI)
import qualified "modern-uri" Text.URI as URI

--------------------------------------------------

import qualified "filepath" System.FilePath as File

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "text" Data.Text as Text
--import           "text" Data.Text (Text)

--------------------------------------------------

import qualified "base" Data.List as List

--------------------------------------------------
--------------------------------------------------

import Prelude_exe

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

type URL = String

--------------------------------------------------
--------------------------------------------------

{-|

-}

data Status

  = Success
  | Failure

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

data Location

  = LocationStdin
  | LocationPath  FilePath
  | LocationURI   URI

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

-- deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| How to fetch (a.k.a download) a location.

TODO When fetching @URL@s, 'FetchBy' specifies an HttpTransport@.

-}

data FetchBy

  = FetchByHaskell
  | FetchByCurl
  | FetchByWget

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultFetchBy'@

instance Default FetchBy where
  def = defaultFetchBy

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

toStatus :: (G.HasField' "status" a Status) => a -> Status
toStatus = G.getField @"status"

--------------------------------------------------

isSuccessful :: Status -> Bool
isSuccessful = \case

  Success -> True
  Failure -> False

--------------------------------------------------

-- | @= 'FetchByHaskell'@

defaultFetchBy :: FetchBy
defaultFetchBy = FetchByHaskell

--------------------------------------------------

{-| 

-}

knownLocations :: [Location]
knownLocations = mconcat

  [ [ LocationStdin ]
  , LocationPath <$> knownLocationPaths
  , LocationURI  <$> knownLocationURIs
  ]

  where

  knownLocationURIs :: [URI]
  knownLocationURIs = concatMap go

    [ "https://github.com/sboosali/skeletor.git"
    ]

     where
     go = Text.pack > URI.mkURI

  knownLocationPaths :: [FilePath]
  knownLocationPaths = filter File.isValid

    [ "~/.config/skeletor"
    ]

--------------------------------------------------

--------------------------------------------------
-- Parsers / Printers ----------------------------
--------------------------------------------------

parseFetchBy :: SimpleParse FetchBy
parseFetchBy =

  mkParserFromPrinterWith (fromJust $ Just "FetchBy") printFetchBy constructors'

--------------------------------------------------

printFetchBy :: SimplePrint FetchBy
printFetchBy = \case

  FetchByHaskell -> "haskell"
  FetchByCurl    -> "curl"
  FetchByWget    -> "wget"

--------------------------------------------------

printedFetchBy :: [String]
printedFetchBy = printFetchBy <$> constructors'

--------------------------------------------------
--------------------------------------------------

printLocation :: SimplePrint Location
printLocation = \case

  LocationStdin      -> "-"
  LocationPath  path -> renderFile path
  LocationURI   uri  -> Text.unpack (URI.render uri)

--------------------------------------------------

{-| 

-}

printedKnownLocations :: [String]
printedKnownLocations = printLocation <$> knownLocations

--------------------------------------------------
--------------------------------------------------

renderFile :: FilePath -> FilePath
renderFile = File.normalise > addPrefix
  where

  addPrefix :: FilePath -> FilePath
  addPrefix path

    | (File.isAbsolute path) = path
    | (File.hasDrive   path) = path
    | (isUserRelative  path) = path

    | otherwise              = "./" <> path

  isUserRelative :: FilePath -> Bool
  isUserRelative = ("~" `List.isPrefixOf`)

--------------------------------------------------
--------------------------------------------------

isTarballPath :: FilePath -> Bool
isTarballPath =

  hasTarballFileExtension

--------------------------------------------------

hasTarballFileExtension :: FilePath -> Bool
hasTarballFileExtension = go

  where

  go path =

      let path' = normalize path
      in

      any (`File.isExtensionOf` path') defaultTarballFileExtensions

  normalize = File.normalise > lowercase

  lowercase = fmap toLower

--------------------------------------------------

defaultTarballFileExtensions :: [String]
defaultTarballFileExtensions =

  [ "tar"

  , "zip"

  , "tgz"
  , "tar.gz"

  , "tar.bz2"
  , "tar.bz"

  ]

--------------------------------------------------
{- Notes -----------------------------------------

--------------------------------------------------
-------------------------------------------------}