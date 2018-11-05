--------------------------------------------------
--------------------------------------------------

{-|

e.g.

@
>>> projectTree <- readProject (Right DefaultHaskellProject)
>>> :i projectTree
@

e.g.

@
>>> findProjectFiles (Right DefaultHaskellProject) >>= traverse_ putStrLn

.\/default
.\/default\/.gitignore
.\/default\/LICENSE.txt
.\/default\/Makefile
.\/default\/README.md
.\/default\/__PACKAGE__
.\/default\/__PACKAGE__\/LICENSE.txt
.\/default\/__PACKAGE__\/__PACKAGE__.cabal
.\/default\/__PACKAGE__\/benches
.\/default\/__PACKAGE__\/benches\/space
.\/default\/__PACKAGE__\/benches\/space\/.dir-locals.el
.\/default\/__PACKAGE__\/benches\/space\/Bench
.\/default\/__PACKAGE__\/benches\/space\/Bench\/__MODULE__.hs
.\/default\/__PACKAGE__\/benches\/space\/Main.hs
.\/default\/__PACKAGE__\/benches\/time
.\/default\/__PACKAGE__\/benches\/time\/.dir-locals.el
.\/default\/__PACKAGE__\/benches\/time\/Bench
.\/default\/__PACKAGE__\/benches\/time\/Bench\/__MODULE__.hs
.\/default\/__PACKAGE__\/benches\/time\/Main.hs
.\/default\/__PACKAGE__\/dist
.\/default\/__PACKAGE__\/dist\/cabal-config-flags
.\/default\/__PACKAGE__\/executables
.\/default\/__PACKAGE__\/executables\/example
.\/default\/__PACKAGE__\/executables\/example\/.dir-locals.el
.\/default\/__PACKAGE__\/executables\/example\/Example
.\/default\/__PACKAGE__\/executables\/example\/Example\/__MODULE__.hs
.\/default\/__PACKAGE__\/executables\/example\/Main.hs
.\/default\/__PACKAGE__\/executables\/program
.\/default\/__PACKAGE__\/executables\/program\/.dir-locals.el
.\/default\/__PACKAGE__\/executables\/program\/Main.hs
.\/default\/__PACKAGE__\/executables\/program\/Prelude_exe.hs
.\/default\/__PACKAGE__\/executables\/program\/__MODULE__
.\/default\/__PACKAGE__\/executables\/program\/__MODULE__\/Main.hs
.\/default\/__PACKAGE__\/executables\/program\/__MODULE__\/Options.hs
.\/default\/__PACKAGE__\/internals
.\/default\/__PACKAGE__\/internals\/.dir-locals.el
.\/default\/__PACKAGE__\/internals\/Internal
.\/default\/__PACKAGE__\/internals\/Internal\/__MODULE__.hs
.\/default\/__PACKAGE__\/internals\/Prelude___PACKAGE_UNDERSCORES__.hs
.\/default\/__PACKAGE__\/library
.\/default\/__PACKAGE__\/library\/.dir-locals.el
.\/default\/__PACKAGE__\/library\/__MODULE__
.\/default\/__PACKAGE__\/library\/__MODULE__\/Core.hs
.\/default\/__PACKAGE__\/library\/__MODULE__\/Derived.hs
.\/default\/__PACKAGE__\/library\/__MODULE__\/Types.hs
.\/default\/__PACKAGE__\/library\/__MODULE__.hs
.\/default\/__PACKAGE__\/tests
.\/default\/__PACKAGE__\/tests\/golden
.\/default\/__PACKAGE__\/tests\/golden\/.dir-locals.el
.\/default\/__PACKAGE__\/tests\/golden\/GoldenTests.hs
.\/default\/__PACKAGE__\/tests\/golden\/Test
.\/default\/__PACKAGE__\/tests\/golden\/Test\/Golden
.\/default\/__PACKAGE__\/tests\/golden\/Test\/Golden\/__MODULE__.hs
.\/default\/__PACKAGE__\/tests\/property
.\/default\/__PACKAGE__\/tests\/property\/.dir-locals.el
.\/default\/__PACKAGE__\/tests\/property\/PropertyTests.hs
.\/default\/__PACKAGE__\/tests\/property\/Test
.\/default\/__PACKAGE__\/tests\/property\/Test\/Property
.\/default\/__PACKAGE__\/tests\/property\/Test\/Property\/__MODULE__.hs
.\/default\/__PACKAGE__\/tests\/unit
.\/default\/__PACKAGE__\/tests\/unit\/.dir-locals.el
.\/default\/__PACKAGE__\/tests\/unit\/Test
.\/default\/__PACKAGE__\/tests\/unit\/Test\/Unit
.\/default\/__PACKAGE__\/tests\/unit\/Test\/Unit\/__MODULE__.hs
.\/default\/__PACKAGE__\/tests\/unit\/UnitTests.hs
.\/default\/cabal.project
.\/default\/documents
.\/default\/documents\/LICENSE-GPL3-StandardLicenseHeader.txt
.\/default\/documents\/LICENSE-GPL3-TerminalLicenseNotice.txt
.\/default\/documents\/LICENSE-GPL3.txt
.\/default\/documents\/NOTES.md
.\/default\/nix
.\/default\/nix\/README.md
.\/default\/nix\/config
.\/default\/nix\/config\/default.nix
.\/default\/nix\/environment.nix
.\/default\/nix\/nixpkgs
.\/default\/nix\/nixpkgs\/default.nix
.\/default\/nix\/nixpkgs\/nixpkgs.json
.\/default\/nix\/nixpkgs\/update-nixpkgs.sh
.\/default\/nix\/overlays
.\/default\/nix\/overlays\/default.nix
.\/default\/nix\/overlays\/ghcs.nix
.\/default\/nix\/overlays\/overlay.nix
.\/default\/nix\/packages.nix
.\/default\/nix\/project.nix
.\/default\/nix\/shell.nix
.\/default\/scripts
.\/default\/scripts\/README.md
.\/default\/scripts\/sboo
.\/default\/scripts\/sboo\/ghcid.sh
.\/default\/shell.nix
@

-}

module Skeletor.Haskell.Core where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Haskell.Types
import Skeletor.Haskell.Variable

--------------------------------------------------

import qualified "filemanip" System.FilePath.Find as Find
import           "filemanip" System.FilePath.Find (FindClause)

import           "filemanip" System.FilePath.GlobPattern (GlobPattern)
import           "filemanip" System.FilePath.GlobPattern ((~~))

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

@
≡ 'readProjectByIdentifier'
@

-}

readProject :: ProjectIdentifier -> IO FileTree
readProject = readProjectByIdentifier

--------------------------------------------------

{-|

@
≡ 'findProjectFilesByIdentifier'
@

-}

findProjectFiles :: ProjectIdentifier -> IO [FilePath]
findProjectFiles = findProjectFilesByIdentifier

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

{-|

@
≡ 'locateProject' '>>>' 'findProjectFilesByPath'
@

-}

findProjectFilesByIdentifier :: ProjectIdentifier -> IO [FilePath]
findProjectFilesByIdentifier = locateProject > findProjectFilesByPath

--------------------------------------------------

{-|

Skips 'ignoredDirectories', when descending.

Skips 'ignoredFiles' and non-files, when collecting.

-}

findProjectFilesByPath :: FilePath -> IO [FilePath]
findProjectFilesByPath = Find.find recursionPredicate filterPredicate

  where

  recursionPredicate :: FindClause Bool
  recursionPredicate = shouldRecurIntoSubdirectory <$> Find.directory

  filterPredicate :: FindClause Bool
  filterPredicate = isRegularFileM Find.&&? isGoodFilenameM
  
    where
    isRegularFileM  = (Find.fileType Find.==? Find.RegularFile)
    isGoodFilenameM = (shouldKeepFilename <$> Find.fileName)

  shouldRecurIntoSubdirectory :: FilePath -> Bool
  shouldRecurIntoSubdirectory = shouldIgnoreDirectory > not

  shouldKeepFilename :: FilePath -> Bool
  shouldKeepFilename = shouldIgnoreFilename > not

  shouldIgnoreDirectory :: FilePath -> Bool
  shouldIgnoreDirectory directory =
    any (directory ~~) ignoredDirectories

  shouldIgnoreFilename :: FilePath -> Bool
  shouldIgnoreFilename filename =
    any (filename ~~) ignoredFiles

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

{-|

@
≡ 'locateProject' '>>>' 'readProjectByPath'
@

-}

readProjectByIdentifier :: ProjectIdentifier -> IO FileTree
readProjectByIdentifier = locateProject > readProjectByPath

--------------------------------------------------

{-|

-}

readProjectByPath :: FilePath -> IO FileTree
readProjectByPath project = do
   files <- findProjectFilesByPath project
   readFileTree files

--------------------------------------------------

{-|

-}

readFileTree :: [FilePath] -> IO FileTree
readFileTree paths = do

  filePathsAndContents <- go `traverse` paths  -- TODO error-handling

  forced <- forceIO filePathsAndContents  -- TODO error-handling

  let wrapped = fromList forced

  pure wrapped

  where

  go path = do
    body <- readFile path
    pure (path, body)

--------------------------------------------------
--------------------------------------------------

{-|

-}

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code




  shouldRecurIntoSubdirectory :: FindClause Bool
  shouldRecurIntoSubdirectory = do
  
      currentDirectory <- Find.directory 
  
      let shouldIgnore = shouldIgnoreDirectory currentDirectory
      let shouldRecur  = not shouldIgnore
  
      pure shouldRecur




UnknownOr KnownProject

Unknown Project 



{-|

-}

findProjectFilesByName :: UnknownOr KnownProject -> IO [FilePath]
findProjectFilesByName = findProjectFilesByPath

  where




-}
--------------------------------------------------