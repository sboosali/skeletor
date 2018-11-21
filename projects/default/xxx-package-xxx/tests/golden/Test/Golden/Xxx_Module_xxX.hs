
--------------------------------------------------
--------------------------------------------------

{-|

See <https://ro-che.info/articles/2017-12-04-golden-tests>.

-}

module Test.Golden.Xxx_Module_xxX where

--------------------------------------------------
--------------------------------------------------

import Xxx_Module_xxX

--------------------------------------------------

import Prelude_xxx_package_xxx

import qualified Paths_xxx_package_xxx as Paths

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "tasty"        Test.Tasty        as Tasty
import qualified "tasty-golden" Test.Tasty.Golden as Tasty

--------------------------------------------------

import qualified "bytestring" Data.ByteString.Lazy as ByteString
import           "bytestring" Data.ByteString.Lazy (ByteString)

--------------------------------------------------

import qualified "filepath" System.FilePath as File

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

dataSubdirectory :: FilePath
dataSubdirectory = "golden"

--------------------------------------------------

-- goldenFileExtensions :: [String]
-- goldenFileExtensions = [".example"]

--------------------------------------------------

dataFileExtensions :: [String]
dataFileExtensions = [".txt"]

--------------------------------------------------

diffCommand :: FilePath -> FilePath -> [String]
diffCommand expected actual =

  ["diff", "-u", expected, actual]

--------------------------------------------------
-- Tests -----------------------------------------
--------------------------------------------------

{-|

-}

goldenTests :: IO Tasty.TestTree
goldenTests = do

  dataDirectory <- Paths.getDataDir

  dataFiles <- Tasty.findByExtension dataFileExtensions dataDirectory

  let tree = goldenTestsWith dataFiles

  return tree

--------------------------------------------------

{-|

-}

goldenTestsWith :: [FilePath] -> Tasty.TestTree
goldenTestsWith dataFiles =

  Tasty.testGroup "[TODO] Golden Tests"

    [ Tasty.goldenVsStringDiff

        testName      -- test name
        diffCommand   -- run « diff » on mismatch
        goldenFile    -- golden file path
        getDataString -- action whose result is tested

    | dataFile <- dataFiles

    , let testName      = ((File.takeBaseName dataFile))
    , let goldenFile    = File.addExtension dataFile "example"

    , let readDataFile  = (ByteString.readFile dataFile)
    , let getDataString = (go <$> readDataFile)

    ]

  where

  -- the function being (golden-)tested:
  go :: ByteString -> ByteString
  go = ByteString.reverse

--------------------------------------------------
{- Notes -----------------------------------------

TODO ByteStringAscii..pack "test:xxx-package-xxx:golden"

« goldenVsStringDiff » is like « goldenVsString »,
while also including the diff in the tasty output.

e.g.:

  goldenTests :: IO Tasty.TestTree
  goldenTests = do

    yamlFiles <- Tasty.findByExtension [".yaml"] "."

    return $ Tasty.testGroup "[TODO] Golden Tests"

      [ Tasty.goldenVsStringDiff

          testFile      -- test name
          diffCommand   -- run `diff` on mismatch
          jsonFile      -- golden file path
          getJsonString -- action whose result is tested

      | yamlFile <- yamlFiles

      , let testFile = (File.takeBaseName yamlFile)
      , let jsonFile = File.replaceExtension yamlFile ".json"
      , let getJsonString = (yamlToJson <$> ByteString.readFile yamlFile)

      ]


-------------------------------------------------}