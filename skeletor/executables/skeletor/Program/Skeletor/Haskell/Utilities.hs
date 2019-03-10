{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DataKinds             #-}

--------------------------------------------------
--------------------------------------------------

{-|

-}

module Program.Skeletor.Haskell.Utilities where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Types
import Program.Skeletor.Haskell.Prelude

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "generic-lens" Data.Generics.Product as G
import qualified "generic-lens" Data.Generics.Sum     as G

--------------------------------------------------

import           "modern-uri" Text.URI (URI)
import qualified "modern-uri" Text.URI as URI

--------------------------------------------------

import qualified "filepath" System.FilePath as File

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------

import qualified "base" Data.List as List

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

toStatus :: (G.HasField' "status" a Status) => a -> Status
toStatus = G.getField @"status"

--------------------------------------------------

isSuccessful :: Status -> Bool
isSuccessful = \case

  Success -> True
  Failure -> False

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

parseLocation :: SimpleParse Location
parseLocation string = go string
  where

  go s

    = (parseConstantLocation s <|> parseVariableLocation s)
    & (maybe (throwM e) return)

  parseConstantLocation :: String -> Maybe Location
  parseConstantLocation = \case

    "-" -> Just LocationStdin
    _   -> Nothing

  parseVariableLocation :: String -> Maybe Location
  parseVariableLocation s

    | (File.isValid s) = Just (LocationPath s)
    | otherwise        = LocationURI <$> URI.mkURI (Text.pack s)

  e = ParseError

      { stringBeingParsed = string
      , thingToParseInto  = "Location"
      }

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
-- Notes -----------------------------------------
--------------------------------------------------
{-



-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------