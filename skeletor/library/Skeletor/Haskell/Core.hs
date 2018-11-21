--------------------------------------------------
--------------------------------------------------

{-| Core types, functions, and constants.

-}

module Skeletor.Haskell.Core where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Haskell.Types
import Skeletor.Haskell.Variable

--------------------------------------------------

import           "filemanip" System.FilePath.GlobPattern (GlobPattern)

--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
--------------------------------------------------

allKnownProjects :: [KnownProject]
allKnownProjects = genum

--------------------------------------------------

knownProjectNames :: [String]
knownProjectNames = printKnownProject <$> allKnownProjects

--------------------------------------------------

-- |
-- >>> defaultProjectName
-- "default"
defaultProjectName :: String
defaultProjectName = printKnownProject defaultKnownProject

--------------------------------------------------
--------------------------------------------------

{-|

-}

locateProject :: ProjectIdentifier -> FilePath
locateProject = either id locateKnownProject

--------------------------------------------------

{-|

-}

locateKnownProject :: KnownProject -> FilePath
locateKnownProject = \case

  DefaultHaskellProject -> "/home/sboo/haskell/skeletor/projects/default"     -- TODO data-files

--------------------------------------------------

{-|

-}

printKnownProject :: KnownProject -> String
printKnownProject = \case

  DefaultHaskellProject -> "default"

--------------------------------------------------

{-|

-}

parseKnownProject :: String -> Maybe KnownProject
parseKnownProject = \case

  "default" -> return DefaultHaskellProject

--------------------------------------------------
--------------------------------------------------

-- | from my standard @.gitignore@.

ignoredDirectories :: [GlobPattern]
ignoredDirectories =
  [ "dist"
  , "dist-*"
  , ".stack-work"
  , ".cabal-sandbox"
  , "result"
  , "result-*"
  ]

--------------------------------------------------

-- | from my standard @.gitignore@.

ignoredFiles :: [GlobPattern]
ignoredFiles = concat
  [ haskellFiles
  , emacsFiles
  , nixFiles
  ]

  where

  haskellFiles =
    [ ".ghc.environment.*"
    , "cabal.project.local"
    , "cabal.sandbox.config"
    , "*.o"
    , "*.hi"
    , "*.chi"
    , "*.chs.h"
    ]

  emacsFiles =
    [ "*~"
    , "\\#*"
    , "\\.\\#*"
    , "\\#*\\#"
    , "TAGS"
    ]

  nixFiles =
    [ "result"
    ]

--------------------------------------------------
--------------------------------------------------





--------------------------------------------------
--------------------------------------------------
