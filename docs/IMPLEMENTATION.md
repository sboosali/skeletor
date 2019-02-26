# Implementation Notes

```haskell
```


## Style

most imports are qualified:

```haskell
import qualified "" _ as _
import           "" _      ()
```



## `base`


### `Data.Maybe`

```haskell
catMaybes :: [Maybe a] -> [a]

listToMaybe :: [a] -> Maybe a
```


### `Data.Monoid`

```haskell
newtype Alt f a

-- Monoid under <|>.
```

### `Data.List`

```haskell
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
```

> The `isSubsequenceOf` function takes two lists and returns True if all the elements of the first list occur, in order, in the second. The elements do not have to occur consecutively.

Laws:

`isSubsequenceOf x y` ≡ `elem x (subsequences y)`

Examples:

```haskell
>>> isSubsequenceOf "GHC" "The Glorious Haskell Compiler"
True

>>> isSubsequenceOf ['a','d'..'z'] ['a'..'z']
True

>>> isSubsequenceOf [1..10] [10,9..0]
False
```

### `System.IO`

```haskell
getLine :: IO String

-- Read a line from the standard input device (same as hGetLine stdin).
```

```haskell
putStr :: String -> IO ()

-- Write a string to the standard output device (same as hPutStr stdout).
```



### `Control.Exception`

```haskell
catches :: IO a -> [Handler a] -> IO a 

Sometimes you want to catch two different sorts of exception. You could do something like

>>> f = expr `catch` \ (ex :: ArithException) -> handleArith ex
>>>          `catch` \ (ex :: IOException)    -> handleIO    ex

However, there are a couple of problems with this approach. The first is that having two exception handlers is inefficient. However, the more serious issue is that the second exception handler will catch exceptions in the first, e.g. in the example above, if handleArith throws an IOException then the second exception handler will catch it.

Instead, we provide a function catches, which would be used thus:

>>> f = expr `catches` [Handler (\ (ex :: ArithException) -> handleArith ex),
>>>                     Handler (\ (ex :: IOException)    -> handleIO    ex)]
```

```haskell
data Handler a =

  Exception e => Handler (e -> IO a)	 

You need this when using catches.

```


















## `containers`

```haskell
foldMapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m 
```





















## `zlib`

```haskell
module Codec.Compression.GZip
```

```haskell
decompress :: ByteString -> ByteString

Decompress a stream of data in the gzip format.

There are a number of errors that can occur. In each case an exception will be thrown. The possible error conditions are:

- if the stream does not start with a valid gzip header
- if the compressed stream is corrupted
- if the compressed stream ends permaturely

Note that the decompression is performed lazily. Errors in the data stream may not be detected until the end of the stream is demanded (since it is only at the end that the final checksum can be checked). If this is important to you, you must make sure to consume the whole decompressed stream before doing any IO action that depends on it.
```

```haskell
decompressWith :: DecompressParams -> ByteString -> ByteString

Like decompress but with the ability to specify various decompression parameters. Typical usage:

>>> decompressWith defaultCompressParams { ... }
```





















## `filepath`


### API

#### ``

```haskell
```

> 

```haskell
```


#### `normalise`

```haskell
normalise :: FilePath -> FilePath
```

normalization:

* `//`, outside of the drive, can be made blank
* `/` becomes `pathSeparator`
* `./ becomes `""`

```haskell
>>> normalise "."                        ==  "."                       --

>>> normalise "/file/\\test////"         ==  "/file/\\test/"           -- Posix
>>> normalise "/file/./test"             ==  "/file/test"              -- Posix
>>> normalise "/test/file/../bob/fred/"  ==  "/test/file/../bob/fred/" -- Posix
>>> normalise "../bob/fred/"             ==  "../bob/fred/"            -- Posix
>>> normalise "./bob/fred/"              ==  "bob/fred/"               -- Posix

>>> normalise "./"                       ==  "./"                      -- Posix
>>> normalise "./."                      ==  "./"                      -- Posix
>>> normalise "/./"                      ==  "/"                       -- Posix
>>> normalise "/"                        ==  "/"                       -- Posix
>>> normalise "bob/fred/."               ==  "bob/fred/"               -- Posix
>>> normalise "//home"                   ==  "/home"                   -- Posix

>>> normalise "c:\\file/bob\\"           ==  "C:\\file\\bob\\"         -- Windows
>>> normalise "c:\\"                     ==  "C:\\"                    -- Windows
>>> normalise "C:.\\"                    ==  "C:"                      -- Windows
>>> normalise "\\\\server\\test"         ==  "\\\\server\\test"        -- Windows
>>> normalise "//server/test"            ==  "\\\\server\\test"        -- Windows
>>> normalise "c:/file"                  ==  "C:\\file"                -- Windows
>>> normalise "/file"                    ==  "\\file"                  -- Windows
>>> normalise "\\"                       ==  "\\"                      -- Windows
>>> normalise "/./"                      ==  "\\"                      -- Windows
```

#### `isAbsolute`

```haskell
isAbsolute :: FilePath -> Bool

isAbsolute = not . isRelative
```

#### `isRelative`

```haskell
isRelative :: FilePath -> Bool
```

> Is a path relative, or is it fixed to the root?

```haskell
>>> isRelative "test/path"        ==  True   -- Posix
>>> isRelative "/test"            ==  False  -- Posix
>>> isRelative "/"                ==  False  -- Posix

>>> isRelative "path\\test"       ==  True   -- Windows
>>> isRelative "c:\\test"         ==  False  -- Windows
>>> isRelative "c:test"           ==  True   -- Windows
>>> isRelative "c:\\"             ==  False  -- Windows
>>> isRelative "c:/"              ==  False  -- Windows
>>> isRelative "c:"               ==  True   -- Windows
>>> isRelative "\\\\foo"          ==  False  -- Windows
>>> isRelative "\\\\?\\foo"       ==  False  -- Windows
>>> isRelative "\\\\?\\UNC\\foo"  ==  False  -- Windows
>>> isRelative "/foo"             ==  True   -- Windows
>>> isRelative "\\foo"            ==  True   -- Windows
```

#### `isValid`

```haskell
isValid :: FilePath -> Bool
```

> Is a FilePath valid, i.e. could you create a file like it? This function checks for invalid names, and invalid characters, but does not check if length limits are exceeded, as these are typically filesystem dependent.

```haskell
>>> isValid ""                   ==  False         --
>>> isValid "\0"                 ==  False         --

>>> isValid "/random_ path:*"    ==  True          -- Posix
>>> isValid x                    ==  not (null x)  -- Posix

>>> isValid "c:\\test"           ==  True          -- Windows
>>> isValid "c:\\test:of_test"   ==  False         -- Windows
>>> isValid "test*"              ==  False         -- Windows
>>> isValid "c:\\test\\nul"      ==  False         -- Windows
>>> isValid "c:\\test\\prn.txt"  ==  False         -- Windows
>>> isValid "c:\\nul\\file"      ==  False         -- Windows
>>> isValid "\\\\"               ==  False         -- Windows
>>> isValid "\\\\\\foo"          ==  False         -- Windows
>>> isValid "\\\\?\\D:file"      ==  False         -- Windows
>>> isValid "foo\tbar"           ==  False         -- Windows
>>> isValid "nul .txt"           ==  False         -- Windows
>>> isValid " nul.txt"           ==  True          -- Windows
```

#### `takeExtensions`

```haskell
takeExtensions :: FilePath -> String
```

> Get all extensions.

```haskell
>>> takeExtensions "/directory/path.ext"  ==  ".ext"
>>> takeExtensions "file.tar.gz"          ==  ".tar.gz"
```

#### `isExtensionOf`

```haskell
isExtensionOf :: String -> FilePath -> Bool
```

> Does the given filename have the specified extension?

```haskell
>>> "png"           `isExtensionOf` "/directory/file.png"      ==  True
>>> ".png"          `isExtensionOf` "/directory/file.png"      ==  True

>>> ".tar.gz"       `isExtensionOf` "bar/foo.tar.gz"           ==  True
>>> "ar.gz"         `isExtensionOf` "bar/foo.tar.gz"           ==  False

>>> "png"           `isExtensionOf` "/directory/file.png.jpg"  ==  False
>>> "csv/table.csv" `isExtensionOf` "/data/csv/table.csv"      ==  False
```

#### `hasExtension`

```haskell
hasExtension :: FilePath -> Bool
```

> Does the given filename have an extension?

```haskell
>>> hasExtension "/directory/path.ext"  ==  True
>>> hasExtension "/directory/path"      ==  False
>>> null (takeExtension x)              ==  not (hasExtension x)
```

### 



























## `turtle`

`Shell` ≡ `[] + IO + Managed`

the `Shell` Embeddings:

```haskell
select ::        [a] -> Shell a
liftIO ::      IO a  -> Shell a
using  :: Managed a  -> Shell a
```

`Shell` Embeddings obey these laws. Associativity Law:

```haskell
do { x <- select m; select (f x) } = select (do { x <- m; f x })
do { x <- liftIO m; liftIO (f x) } = liftIO (do { x <- m; f x })
do { x <- with   m; using  (f x) } = using  (do { x <- m; f x })
```

and Identity Law:

```haskell
select (return x) = return x
liftIO (return x) = return x
using  (return x) = return x
```

> ... and select obeys these additional laws:

```haskell
select xs <|> select ys = select (xs <|> ys)
select empty = empty
```




## `gitlib`

<http://hackage.haskell.org/package/gitlib-3.1.2/docs/Git-Tutorial.html>

## `hlibgit2`

Haskell Bindings to the @libgit@ C Library.

<https://github.com/jwiegley/gitlib/tree/master/hlibgit2>

```haskell
-- | type of a git object.

data ObjectType =
      TypeTree
    | TypeBlob
    | TypeCommit
    | TypeTag
    | TypeDeltaOff
    | TypeDeltaRef
```

```haskell
-- | Git object file type

data ObjectFileType =
      FileTypeDirectory
    | FileTypeRegularFile
    | FileTypeSymbolicLink
    | FileTypeGitLink
```

```haskell
-- | traditional unix permission for owner, group and permissions

data FilePermissions = FilePermissions
    { getOwnerPerm :: {-# UNPACK #-} !Perm
    , getGroupPerm :: {-# UNPACK #-} !Perm
    , getOtherPerm :: {-# UNPACK #-} !Perm
    }
    
-- | a bitfield representing a typical unix permission:
-- * bit 0 represents the read permission
-- * bit 1 represents the write permission
-- * bit 2 represents the execute permission

type Perm = Word8
```

```haskell
-- | an author or committer line
-- has the format: name <email> time timezone
-- FIXME: should be a string, but I don't know if the data is stored
-- consistantly in one encoding (UTF8)

data Person = Person
    { personName  :: !ByteString
    , personEmail :: !ByteString
    , personTime  :: !GitTime
    }
```

```haskell
-- | Represent a commit object.

data Commit hash = Commit
    { commitTreeish   :: !(Ref hash)
    , commitParents   :: [Ref hash]
    , commitAuthor    :: !Person
    , commitCommitter :: !Person
    , commitEncoding  :: Maybe ByteString
    , commitExtras    :: [CommitExtra]
    , commitMessage   :: !ByteString
    } deriving (Show,Eq)

data CommitExtra = CommitExtra
    { commitExtraKey   :: !ByteString
    , commitExtraValue :: !ByteString
    } deriving (Show,Eq)
```

```haskell
-- | Represent a signed tag.

data Tag hash = Tag
    { tagRef        :: !(Ref hash)
    , tagObjectType :: !ObjectType
    , tagBlob       :: !ByteString
    , tagName       :: !Person
    , tagS          :: !ByteString
    } deriving (Show,Eq)
```





## `network-uri`

### `Network.URI

```haskell
-- |Represents a general universal resource identifier using
--  its component parts.
--
--  For example, for the URI
--
--  >   foo://anonymous@www.haskell.org:42/ghc?query#frag
--
--  the components are:
--

data URI = URI

    { uriScheme     :: String           -- ^ @foo:@
    , uriAuthority  :: Maybe URIAuth    -- ^ @\/\/anonymous\@www.haskell.org:42@
    , uriPath       :: String           -- ^ @\/ghc@
    , uriQuery      :: String           -- ^ @?query@
    , uriFragment   :: String           -- ^ @#frag@
    }
```

```haskell
-- |Type for authority value within a URI

data URIAuth = URIAuth

    { uriUserInfo   :: String           -- ^ @anonymous\@@
    , uriRegName    :: String           -- ^ @www.haskell.org@
    , uriPort       :: String           -- ^ @:42@
    }
```

```haskell
-- |Blank URI

nullURI :: URI
nullURI = URI

    { uriScheme     = ""
    , uriAuthority  = Nothing
    , uriPath       = ""
    , uriQuery      = ""
    , uriFragment   = ""
    }
```






## `Cabal`

### module `Distribution.Simple.Program`

```haskell
-- | Represents a program which can be configured.
--
-- Note: rather than constructing this directly, start with 'simpleProgram' and
-- override any extra fields.
--

data Program = Program {

       -- | The simple name of the program, eg. ghc

       programName :: String,

       -- | A function to search for the program if its location was not
       -- specified by the user. Usually this will just be a call to
       -- 'findProgramOnSearchPath'.
       --
       -- It is supplied with the prevailing search path which will typically
       -- just be used as-is, but can be extended or ignored as needed.
       --
       -- For the purpose of change monitoring, in addition to the location
       -- where the program was found, it returns all the other places that
       -- were tried.
       --

       programFindLocation :: Verbosity -> ProgramSearchPath
                              -> IO (Maybe (FilePath, [FilePath])),

       -- | Try to find the version of the program. For many programs this is
       -- not possible or is not necessary so it's OK to return Nothing.

       programFindVersion :: Verbosity -> FilePath -> IO (Maybe Version),

       -- | A function to do any additional configuration after we have
       -- located the program (and perhaps identified its version). For example
       -- it could add args, or environment vars.

       programPostConf :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram,

       -- | A function that filters any arguments that don't impact the output
       -- from a commandline. Used to limit the volatility of dependency hashes
       -- when using new-build.

       programNormaliseArgs :: Maybe Version -> PackageDescription -> [String] -> [String]
     }
```


```haskell
-- | Represents a program which has been configured and is thus ready to be run.
--
-- These are usually made by configuring a 'Program', but if you have to
-- construct one directly then start with 'simpleConfiguredProgram' and
-- override any extra fields.
--

data ConfiguredProgram = ConfiguredProgram {

       -- | Just the name again
       --
       programId :: String,

       -- | The version of this program, if it is known.
       --
       programVersion :: Maybe Version,

       -- | Default command-line args for this program.
       -- These flags will appear first on the command line, so they can be
       -- overridden by subsequent flags.
       --
       programDefaultArgs :: [String],

       -- | Override command-line args for this program.
       -- These flags will appear last on the command line, so they override
       -- all earlier flags.
       --
       programOverrideArgs :: [String],

       -- | Override environment variables for this program.
       -- These env vars will extend\/override the prevailing environment of
       -- the current to form the environment for the new process.
       --
       programOverrideEnv :: [(String, Maybe String)],

       -- | A key-value map listing various properties of the program, useful
       -- for feature detection. Populated during the configuration step, key
       -- names depend on the specific program.
       --
       programProperties :: Map.Map String String,

       -- | Location of the program. eg. @\/usr\/bin\/ghc-6.4@
       --
       programLocation :: ProgramLocation,

       -- | In addition to the 'programLocation' where the program was found,
       -- these are additional locations that were looked at. The combination
       -- of ths found location and these not-found locations can be used to
       -- monitor to detect when the re-configuring the program might give a
       -- different result (e.g. found in a different location).
       --
       programMonitorFiles :: [FilePath]

     }
```

```haskell
-- | Where a program was found. Also tells us whether it's specified by user or
-- not.  This includes not just the path, but the program as well.

data ProgramLocation

    = UserSpecified { locationPath :: FilePath }
      -- ^The user gave the path to this program,
      -- eg. --ghc-path=\/usr\/bin\/ghc-6.6

    | FoundOnSystem { locationPath :: FilePath }
```

```haskell
simpleProgram :: String -> Program
simpleProgram name = Program

 {
    programName         = name,
    programFindLocation = \v p -> findProgramOnSearchPath v p name,
    programFindVersion  = \_ _ -> return Nothing,
    programPostConf     = \_ p -> return p,
    programNormaliseArgs   = \_ _ -> id
  }
```


### module `Distribution.Simple.Program.Find`

```haskell
import qualified System.Directory as Directory
         ( findExecutable )

import System.FilePath as FilePath
         ( (</>), (<.>), splitSearchPath, searchPathSeparator, getSearchPath
         , takeDirectory )

```


```haskell
-- | A search path to use when locating executables. This is analogous
-- to the unix @$PATH@ or win32 @%PATH%@ but with the ability to use
-- the system default method for finding executables ('findExecutable' which
-- on unix is simply looking on the @$PATH@ but on win32 is a bit more
-- complicated).
--
-- The default to use is @[ProgSearchPathDefault]@ but you can add extra dirs
-- either before, after or instead of the default, e.g. here we add an extra
-- dir to search after the usual ones.
--
-- > ['ProgramSearchPathDefault', 'ProgramSearchPathDir' dir]
--

type ProgramSearchPath = [ProgramSearchPathEntry]

data ProgramSearchPathEntry =
         ProgramSearchPathDir FilePath  -- ^ A specific dir
       | ProgramSearchPathDefault       -- ^ The system default

defaultProgramSearchPath :: ProgramSearchPath
defaultProgramSearchPath = [ProgramSearchPathDefault]
```


```haskell
findProgramOnSearchPath :: Verbosity -> ProgramSearchPath
                        -> FilePath -> IO (Maybe (FilePath, [FilePath]))

findProgramOnSearchPath verbosity searchpath prog = do

    debug verbosity $ "Searching for " ++ prog ++ " in path."
    res <- tryPathElems [] searchpath
    case res of
      Nothing   -> debug verbosity ("Cannot find " ++ prog ++ " on the path")
      Just (path, _) -> debug verbosity ("Found " ++ prog ++ " at "++ path)
    return res

  where

    tryPathElems :: [[FilePath]] -> [ProgramSearchPathEntry]
                 -> IO (Maybe (FilePath, [FilePath]))
    tryPathElems _     []       = return Nothing
    tryPathElems tried (pe:pes) = do
      res <- tryPathElem pe
      case res of
        (Nothing,      notfoundat) -> tryPathElems (notfoundat : tried) pes
        (Just foundat, notfoundat) -> return (Just (foundat, alltried))
          where
            alltried = concat (reverse (notfoundat : tried))

    tryPathElem :: ProgramSearchPathEntry -> NoCallStackIO (Maybe FilePath, [FilePath])
    tryPathElem (ProgramSearchPathDir dir) =
        findFirstExe [ dir </> prog <.> ext | ext <- exeExtensions ]

    -- On windows, getSystemSearchPath is not guaranteed 100% correct so we
    -- use findExecutable and then approximate the not-found-at locations.
    tryPathElem ProgramSearchPathDefault | buildOS == Windows = do
      mExe    <- firstJustM [ findExecutable (prog <.> ext) | ext <- exeExtensions ]
      syspath <- getSystemSearchPath
      case mExe of
        Nothing ->
          let notfoundat = [ dir </> prog | dir <- syspath ] in
          return (Nothing, notfoundat)

        Just foundat -> do
          let founddir   = takeDirectory foundat
              notfoundat = [ dir </> prog
                           | dir <- takeWhile (/= founddir) syspath ]
          return (Just foundat, notfoundat)

    -- On other OSs we can just do the simple thing
    tryPathElem ProgramSearchPathDefault = do
      dirs <- getSystemSearchPath
      findFirstExe [ dir </> prog <.> ext | dir <- dirs, ext <- exeExtensions ]

    findFirstExe :: [FilePath] -> NoCallStackIO (Maybe FilePath, [FilePath])
    findFirstExe = go []
      where
        go fs' []     = return (Nothing, reverse fs')
        go fs' (f:fs) = do
          isExe <- doesExecutableExist f
          if isExe
            then return (Just f, reverse fs')
            else go (f:fs') fs

    -- Helper for evaluating actions until the first one returns 'Just'
    firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
    firstJustM [] = return Nothing
    firstJustM (ma:mas) = do
      a <- ma
      case a of
        Just _  -> return a
        Nothing -> firstJustM mas
```


```haskell

```




## `cabal-install`

### module `Distribution.Client.VCS`

```haskell
-- | VCS driver for Git.
--
vcsGit :: VCS Program
vcsGit =
    VCS {
      vcsRepoType = Git,
      vcsProgram  = gitProgram,
      vcsCloneRepo,
      vcsSyncRepos
    }
  where
    vcsCloneRepo :: Verbosity
                 -> ConfiguredProgram
                 -> SourceRepo
                 -> FilePath
                 -> FilePath
                 -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcuri destdir =
        [ programInvocation prog cloneArgs ]
        -- And if there's a tag, we have to do that in a second step:
     ++ [ (programInvocation prog (checkoutArgs tag)) {
            progInvokeCwd = Just destdir
          }
        | tag <- maybeToList (repoTag repo) ]
      where
        cloneArgs  = ["clone", srcuri, destdir]
                     ++ branchArgs ++ verboseArg
        branchArgs = case repoBranch repo of
          Just b  -> ["--branch", b]
          Nothing -> []
        checkoutArgs tag = "checkout" : verboseArg ++ [tag, "--"]
        verboseArg = [ "--quiet" | verbosity < Verbosity.normal ]

    vcsSyncRepos :: Verbosity
                 -> ConfiguredProgram
                 -> [(SourceRepo, FilePath)]
                 -> IO [MonitorFilePath]
    vcsSyncRepos _ _ [] = return []
    vcsSyncRepos verbosity gitProg
                 ((primaryRepo, primaryLocalDir) : secondaryRepos) = do

      vcsSyncRepo verbosity gitProg primaryRepo primaryLocalDir Nothing
      sequence_
        [ vcsSyncRepo verbosity gitProg repo localDir (Just primaryLocalDir)
        | (repo, localDir) <- secondaryRepos ]
      return [ monitorDirectoryExistence dir 
             | dir <- (primaryLocalDir : map snd secondaryRepos) ]

    vcsSyncRepo verbosity gitProg SourceRepo{..} localDir peer = do
        exists <- doesDirectoryExist localDir
        if exists
          then git localDir                 ["fetch"]
          else git (takeDirectory localDir) cloneArgs
        git localDir checkoutArgs
      where
        git :: FilePath -> [String] -> IO ()
        git cwd args = runProgramInvocation verbosity $
                         (programInvocation gitProg args) {
                           progInvokeCwd = Just cwd
                         }

        cloneArgs      = ["clone", "--no-checkout", loc, localDir]
                      ++ case peer of
                           Nothing           -> []
                           Just peerLocalDir -> ["--reference", peerLocalDir]
                      ++ verboseArg
                         where Just loc = repoLocation
        checkoutArgs   = "checkout" : verboseArg ++ ["--detach", "--force"
                         , checkoutTarget, "--" ]
        checkoutTarget = fromMaybe "HEAD" (repoBranch `mplus` repoTag)
        verboseArg     = [ "--quiet" | verbosity < Verbosity.normal ]
```

```haskell
gitProgram :: Program
gitProgram = (simpleProgram "git") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      case words str of
        -- "git version 2.5.5"
        (_:_:ver:_) | all isTypical ver -> ver

        -- or annoyingly "git version 2.17.1.windows.2" yes, really
        (_:_:ver:_) -> intercalate "."
                     . takeWhile (all isNum)
                     . split
                     $ ver
        _ -> ""
  }
  where
    isNum     c = c >= '0' && c <= '9'
    isTypical c = isNum c || c == '.'
    split    cs = case break (=='.') cs of
                    (chunk,[])     -> chunk : []
                    (chunk,_:rest) -> chunk : split rest
```




## `parsers`

### module `Text.Parser.Combinators`

```haskell

-- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
-- equal to zero, the parser equals to @return []@. Returns a list of
-- @n@ values returned by @p@.

count :: Applicative m => Int -> m a -> m [a]

count n p | n <= 0    = pure []
          | otherwise = sequenceA (replicate n p)
```








## `tar` package

### Example

```haskell
import qualified "tar" Codec.Archive.Tar as Tar


```

### Modules

`Codec.Archive.Tar`:

> Reading, writing and manipulating ".tar" archive files.

### Functions

#### `extract`

    extract
        :: FilePath	-- Destination directory
        -> FilePath	-- Tarball
        -> IO ()	 


>Extract all the files contained in a ".tar" file.
>
>It is equivalent to calling the standard tar program like so:

    $ tar -x -f tarball.tar -C dir


>So for example if the tarball.tar file contains foo/bar.txt then this will extract it to dir/foo/bar.txt.
>
>This is a high level "all in one" operation. Since you may need variations on this function it is instructive to see how it is written. It is just:

    Tar.unpack dir . Tar.read =<< BS.readFile tar

#### `unpack`

    unpack
        :: Exception e
        => FilePath     -- ^ Destination directory
        -> Entries e
        -> IO ()

> Create local files and directories based on the entries of a tar archive.
> 
> This is a portable implementation of unpacking suitable for portable archives. It handles NormalFile and Directory entries and has simulated support for SymbolicLink and HardLink entries. Links are implemented by copying the target file. This therefore works on Windows as well as Unix. All other entry types are ignored, that is they are not unpacked and no exception is raised.
> 
> If the Entries ends in an error then it is raised an an exception. Any files or directories that have been unpacked before the error was encountered will not be deleted. For this reason you may want to unpack into an empty directory so that you can easily clean up if unpacking fails part-way.
> 
> On its own, this function only checks for security (using checkSecurity). You can do other checks by applying checking functions to the Entries that you pass to this function. For example:

    unpack dir (checkTarbomb expectedDir entries)
    
> If you care about the priority of the reported errors then you may want to use checkSecurity before checkTarbomb or other checks.

#### `read`

    read :: ByteString -> Entries FormatError

> Convert a data stream in the tar file format into an internal data structure. Decoding errors are reported by the Fail constructor of the Entries type

#### `check*`

checks:

    unpack dir (checkTarbomb expectedDir entries)

#### `checkTarbomb`

    checkTarbomb :: FilePath -> Entries e -> Entries (Either e TarBombError)

> This function checks a sequence of tar entries for being a "tar bomb". This means that the tar file does not follow the standard convention that all entries are within a single subdirectory, e.g. a file "foo.tar" would usually have all entries within the "foo/" subdirectory.
> 
> Given the expected subdirectory, this function checks all entries are within that subdirectroy.
> 
> Note: This check must be used in conjunction with checkSecurity (or checkPortability).

### Notes

#### Compression

Compressed tar archives, via `zlib` package:

>Tar files are commonly used in conjunction with gzip compression, as in ".tar.gz" or ".tar.bz2" files. This module does not directly handle compressed tar files however they can be handled easily by composing functions from this module and the modules Codec.Compression.GZip or Codec.Compression.BZip (see zlib or bzlib packages).
>
>Creating a compressed ".tar.gz" file is just a minor variation on the create function, but where throw compression into the pipeline:

    BS.writeFile tar . GZip.compress . Tar.write =<< Tar.pack base dir


>Similarly, extracting a compressed ".tar.gz" is just a minor variation on the extract function where we use decompression in the pipeline:

    Tar.unpack dir . Tar.read . GZip.decompress =<< BS.readFile tar








## `temporary`

```ghci
λ> getCanonicalTemporaryDirectory

   "/tmp"
   it :: FilePath

λ> createTempDirectory "/tmp" "xyz"

   "/tmp/xyz-52e768a90eb24cb6"
   it :: FilePath

λ> createTempDirectory "/tmp" "xyz"

   "/tmp/xyz-10a16da732fbd79d"
   it :: FilePath
```

```haskell
-- | Create a temporary directory.

createTempDirectory

  :: FilePath    -- ^ Parent directory to create the directory in

  -> String      -- ^ Directory name template

  -> IO FilePath

createTempDirectory dir template = findTempName

  where

    findTempName = do
      x :: Word <- randomIO
      let dirpath = dir </> template ++ printf "-%.*x" (wordSize `div` 4) x
      r <- MC.try $ mkPrivateDir dirpath
      case r of
        Right _ -> return dirpath
        Left  e | isAlreadyExistsError e -> findTempName
                | otherwise              -> ioError e
```

```haskell
-- | Create and use a temporary directory inside the given directory.
--
-- The directory is deleted after use.

withTempDirectory :: (MC.MonadMask m, MonadIO m) =>
                     FilePath -- ^ Parent directory to create the directory in
                  -> String   -- ^ Directory name template
                  -> (FilePath -> m a) -- ^ Callback that can use the directory
                  -> m a

withTempDirectory targetDir template =
  MC.bracket
    (liftIO (createTempDirectory targetDir template))
    (liftIO . ignoringIOErrors . removeDirectoryRecursive)
```









## `ansi-terminal`.

### module `System.Console.ANSI`

### Types

```haskell
data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White


           deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)
```

```haskell
data ColorIntensity = Dull
                    | Vivid


                    deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)
```

```haskell
data ConsoleLayer = Foreground
                  | Background


                  deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)
```

```haskell
-- | ANSI blink speeds: values other than 'NoBlink' are not widely supported


data BlinkSpeed = SlowBlink -- ^ Less than 150 blinks per minute
                | RapidBlink -- ^ More than 150 blinks per minute
                | NoBlink


                deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)
```

```haskell
data Underlining
  = SingleUnderline
  -- | Not widely supported. Not supported natively on Windows 10
  | DoubleUnderline
  | NoUnderline


  deriving (Eq, Ord, Bounded ,Enum, Show, Read, Ix)
```

```haskell
-- | ANSI general console intensity: usually treated as setting the font style
-- (e.g. 'BoldIntensity' causes text to be bold)


data ConsoleIntensity
  = BoldIntensity
  -- | Not widely supported: sometimes treated as concealing text. Not supported
  -- natively on Windows 10
  | FaintIntensity
  | NormalIntensity


  deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)
```

ANSI Select Graphic Rendition (SGR) command:

```haskell
data SGR
  -- | Default rendition, cancels the effect of any preceding occurrence of SGR
  -- (implementation-defined)
  = Reset
  -- | Set the character intensity. Partially supported natively on Windows 10
  | SetConsoleIntensity !ConsoleIntensity
  -- | Set italicized. Not widely supported: sometimes treated as swapping
  -- foreground and background. Not supported natively on Windows 10
  | SetItalicized !Bool
  -- | Set or clear underlining. Partially supported natively on Windows 10
  | SetUnderlining !Underlining
  -- | Set or clear character blinking. Not supported natively on Windows 10
  | SetBlinkSpeed !BlinkSpeed
  -- | Set revealed or concealed. Not widely supported. Not supported natively
  -- on Windows 10
  | SetVisible !Bool
  -- | Set negative or positive image. Supported natively on Windows 10
  | SetSwapForegroundBackground !Bool
  -- | Set a color from the standard palette of 16 colors (8 colors by 2
  -- color intensities). Many terminals allow the palette colors to be
  -- customised
  | SetColor !ConsoleLayer !ColorIntensity !Color
  -- | Set a true color (24 bit color depth). Supported natively on Windows 10
  -- from the Creators Update (April 2017)
  --
  -- @since 0.7
  | SetRGBColor !ConsoleLayer !(Colour Float)
  -- | Set a color from a palette of 256 colors using a numerical index
  -- (0-based). Supported natively on Windows 10 from the Creators Update (April
  -- 2017) but not on legacy Windows native terminals. See 'xtermSystem',
  -- 'xterm6LevelRGB' and 'xterm24LevelGray' to construct indices based on
  -- xterm's standard protocol for a 256-color palette.
  --
  -- @since 0.9
  | SetPaletteColor !ConsoleLayer !Word8


  deriving (Eq, Show, Read)
```










## `ansi-wl-pprint`

### `nest`

`nest :: Int -> Doc -> Doc`:

> The document (nest i x) renders document x with the current indentation level increased by i.

for example, this documents:

```
nest 2 (text "hello" <$> text "world") <$> text "!"
```

renders as:

```
hello
  world
!
```

### ANSI Docs

ANSI formatting combinators

> This terminal formatting functionality is, as far as possible, portable across platforms with their varying terminals. However, note that to display ANSI colors and formatting will only be displayed on Windows consoles if the Doc value is output using the putDoc function or one of its friends. Rendering the Doc to a String and then outputting that will only work on Unix-style operating systems.

```
Forecolor combinators
black :: Doc -> Doc 

Displays a document with the black forecolor

red :: Doc -> Doc 

Displays a document with the red forecolor

green :: Doc -> Doc 

Displays a document with the green forecolor

yellow :: Doc -> Doc 

Displays a document with the yellow forecolor

blue :: Doc -> Doc 

Displays a document with the blue forecolor

magenta :: Doc -> Doc 

Displays a document with the magenta forecolor

cyan :: Doc -> Doc 

Displays a document with the cyan forecolor

white :: Doc -> Doc 

Displays a document with the white forecolor

dullblack :: Doc -> Doc 

Displays a document with the dull black forecolor
```

Emboldening combinators:

```
bold :: Doc -> Doc 

Displays a document in a heavier font weight

debold :: Doc -> Doc 

Displays a document in the normal font weight
```


Underlining combinators:

```
underline :: Doc -> Doc 

Displays a document with underlining

deunderline :: Doc -> Doc 

Displays a document with no underlining
```

Formatting elimination combinators:


```
plain :: Doc -> Doc 

Removes all colorisation, emboldening and underlining from a document
```

### Implementation

see `ansi-terminal`.




















## `optparse-applicative`

### Links

* <https://github.com/pcapriotti/optparse-applicative/blob/master/README.md>

* <http://hackage.haskell.org/package/optparse-applicative-0.14.2.0/docs/Options-Applicative.html>

* <https://medium.com/@danidiaz/subcommands-with-optparse-applicative-1234549b21c6>

### Code

####

```haskell
switch = flag False True
```

####

```haskell
eitherReader :: (String -> Either String a) -> ReadM a
```

Convert a function producing an Either into a reader.

As an example, one can create a ReadM from an attoparsec Parser easily with
```

```haskell
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

attoparsecReader :: A.Parser a -> ReadM a
attoparsecReader p = eitherReader (A.parseOnly p . T.pack)
```

####

```haskell
data ParserHelp
	 
helpError       :: Chunk Doc	 
helpSuggestions :: Chunk Doc	 
helpHeader      :: Chunk Doc	 
helpUsage       :: Chunk Doc	 
helpBody        :: Chunk Doc	 
helpFooter      :: Chunk Doc	 
```

```haskell
data ParserPrefs = ParserPrefs

  { prefMultiSuffix     :: String -- ^ metavar suffix for multiple options

  , prefDisambiguate    :: Bool   -- ^ automatically disambiguate abbreviations
                                  -- (default: False)

  , prefShowHelpOnError :: Bool   -- ^ always show help text on parse errors
                                  -- (default: False)

  , prefShowHelpOnEmpty :: Bool   -- ^ show the help text for a command or subcommand
                                  -- if it fails with no input (default: False)

  , prefBacktrack       :: Bool   -- ^ backtrack to parent parser when a
                                  -- subcommand fails (default: True)

  , prefColumns         :: Int    -- ^ number of columns in the terminal, used to
                                  -- format the help page (default: 80)
  }
```

```haskell
data OptName

  = OptShort !Char
  | OptLong  !String
```

`ReadM`:

```haskell
eitherReader :: (String -> Either String a) -> ReadM

-- a Left will hold the error message for a parse failure.

-- e.g.:

parseFluxCapacitor :: ReadM FluxCapacitor
parseFluxCapacitor = eitherReader $ \s -> ...

option parseFluxCapacitor ( long "flux-capacitor" )
```


### Bash, Zsh, and Fish Completions
optparse-applicative

`--bash-completion-script`: this takes the full path of the program as argument, and prints a bash script, which, when sourced into a bash session, will install the necessary machinery to make bash completion work. For a quick test, you can run something like (for a program called foo on the PATH):

    $ source <(foo --bash-completion-script `which foo`)


### Customising the help screen

The progDesc, header, and footer functions can be used to specify a brief description or tagline for the program, and detailed information surrounding the generated option and command descriptions.
Internally we actually use the ansi-wl-pprint library, and one can use the headerDoc combinator and friends if additional customisation is required.






### Command groups

One experimental feature which may be useful for programs with many subcommands is command group separation.

```
data Sample
  = Hello [String]
  | Goodbye
  deriving (Eq, Show)

hello :: Parser Sample
hello = Hello <$> many (argument str (metavar "TARGET..."))

sample :: Parser Sample
sample = subparser

       ( command "hello" (info hello (progDesc "Print greeting"))
      <> command "goodbye" (info (pure Goodbye) (progDesc "Say goodbye"))
       )

      <|> subparser

       ( command "bonjour" (info hello (progDesc "Print greeting"))
      <> command "au-revoir" (info (pure Goodbye) (progDesc "Say goodbye"))
      <> commandGroup "French commands:"
      <> hidden
       )
```

This will logically separate the usage text for the two subparsers (these would normally appear together if the commandGroup modifier was not used). The hidden modifier suppresses the metavariable for the second subparser being show in the brief usage line, which is desirable in some cases.


### 

flag

  :: a	
  default value
  
  -> a	
  active value
  
  -> Mod FlagFields a	
  option modifier
  
  -> Parser a	 
  Builder for a flag parser.
  
A flag that switches from a "default value" to an "active value" when encountered. For a simple boolean value, use switch instead.

Note: Because this parser will never fail, it can not be used with combinators such as some or many, as these combinators continue until a failure occurs. See flag'.

### 

strOption :: IsString s => Mod OptionFields s -> Parser s

Builder for an option taking a String argument.

### 

option :: ReadM a -> Mod OptionFields a -> Parser a

Builder for an option using the given reader.

This is a regular option, and should always have either a long or short name specified in the modifiers (or both).

nameParser = option str ( long "name" <> short 'n' )

### 

`info`
 
`info :: Parser a -> InfoMod a -> ParserInfo a`


### 

`ParseError`:

```haskell
readerAbort :: ParseError -> ReadM a

-- Abort option reader by exiting with a ParseError.

data ParseError

  = ErrorMsg String
  | InfoMsg String
  | ShowHelpText
  | UnknownError
  | MissingError IsCmdStart SomeParser
  | ExpectsArgError String
  | UnexpectedError String SomeParser

instance Monoid ParseError
```

### 

`command`:

```haskell
command :: String -> ParserInfo a -> Mod CommandFields a
```

> Suggested usage for multiple commands is to add them to a single subparser. e.g.

```haskell
command :: String -> ParserInfo a -> Mod CommandFields a

sample :: Parser Sample
sample = subparser
       ( command "hello"
         (info hello (progDesc "Print greeting"))
      <> command "goodbye"
         (info goodbye (progDesc "Say goodbye"))
       )
```

### 

prefs :: PrefsMod -> ParserPrefs

Create a ParserPrefs given a modifier

ParserPrefs	 

  prefMultiSuffix :: String	
  metavar suffix for multiple options
  
  prefDisambiguate :: Bool	
  automatically disambiguate abbreviations (default: False)
  
  prefShowHelpOnError :: Bool	
  always show help text on parse errors (default: False)
  
  prefShowHelpOnEmpty :: Bool	
  show the help text for a command or subcommand if it fails with no input (default: False)
  
  prefBacktrack :: Bool	
  backtrack to parent parser when a subcommand fails (default: True)
  
  prefColumns :: Int	
  number of columns in the terminal, used to format the help page (default: 80)

disambiguate :: PrefsMod

Turn on disambiguation.

See https://github.com/pcapriotti/optparse-applicative#disambiguation

showHelpOnError :: PrefsMod

Show full help text on any error.

showHelpOnEmpty :: PrefsMod

Show the help text if the user enters only the program name or subcommand.

This will suppress a Missing: error and show the full usage instead if a user just types the name of the program.

### 

mkCompleter :: (String -> IO [String]) -> Completer

Smart constructor for a Completer

listIOCompleter :: IO [String] -> Completer

Create a Completer from an IO action

listCompleter :: [String] -> Completer

Create a Completer from a constant list of strings.

bashCompleter :: String -> Completer

Run a compgen completion action.

Common actions include file and directory. See http://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html#Programmable-Completion-Builtins for a complete list.

### 

> Notice that the ArgumentFields type doesn’t have a HasName instance, as it wouldn’t make sense for positional arguments. You can’t use long or short with it. It does have a HasMetavar instance, so you can still use metavar. Conversely, FlagFields doesn’t have a HasMetavar instance.

### 












## `attoparsec`

### module `Data.Attoparsec.Text`

#### 

```haskell
parse :: Parser a -> Text -> Result a
```
#### 

```haskell
takeWhile1 :: (Char -> Bool) -> Parser Text
```

Consume input as long as the predicate returns True, and return the consumed input.

This parser requires the predicate to succeed on at least one character of input: it will fail if the predicate never returns True or if there is no input left.

#### 

```haskell
isEndOfLine :: Char -> Bool
```

A predicate that matches either a carriage return '\r' or newline '\n' character.

#### 

```haskell
isHorizontalSpace :: Char -> Bool
```

A predicate that matches either a space ' ' or horizontal tab '\t' character.

#### 

```haskell
-- | Attempt a parse, and if it fails, rewind the input so that no
-- input appears to have been consumed.
--
-- This combinator is provided for compatibility with Parsec.
-- attoparsec parsers always backtrack on failure.

try :: Parser i a -> Parser i a
try p = p
```

#### 

```haskell

```

#### 

```haskell

```

#### 

```haskell

```

#### 

```haskell

```

#### 

```haskell

```

#### 

```haskell

```

#### 

```haskell

```

#### 

```haskell

```

























## `megaparsec`

### ``

``:

```haskell
```

### module `Text.Megaparsec`

```haskell
```

### Parsing (i.e. Running Parsers)

`runParser`:

```haskell
runParser

  :: Parsec e s a                       -- ^ Parser to run
  -> String                             -- ^ Name of source file
  -> s                                  -- ^ Input for parser
  -> Either (ParseErrorBundle s e) a

runParser p name s = snd $

  runParser' p (initialState name s)
```

`ParseErrorBundle`:


```haskell
data ParseErrorBundle s e 

  ≡
  
  NonEmpty (ParseError s e)
```

`ParseError`:

```haskell
-- | @'ParseError' t e@ represents a parse error parametrized over the token
-- type @t@ and the custom data @e@.
--
-- 'Semigroup' and 'Monoid' instances of the data type allow to merge parse
-- errors from different branches of parsing. When merging two
-- 'ParseError's, the longest match is preferred; if positions are the same,
-- custom data sets and collections of message items are combined. Note that
-- fancy errors take precedence over trivial errors in merging.

data ParseError s e

  = TrivialError Int (Maybe (ErrorItem (Token s))) (Set (ErrorItem (Token s)))

    -- ^ Trivial errors, generated by Megaparsec's machinery. The data
    -- constructor includes the source position of error, unexpected token
    -- (if any), and expected tokens.

  | FancyError Int (Set (ErrorFancy e))

    -- ^ Fancy, custom errors.
```


### `MonadParsec`

`MonadParsec`:

```haskell
class (Stream s, MonadPlus m)

  => MonadParsec e s m
 | m -> e s

  where

  ...
```

`Stream`:

```haskell
class (Ord (Token s), Ord (Tokens s))

  => Stream s

  where

  type Token  s :: *
  type Tokens s :: *
  
  ...
```

`MonadPlus`:

```haskell
class (Alternative m, Monad m) => MonadPlus m where
```

`State`:

```haskell

data State s = State

  { stateInput :: s

    -- ^ The rest of input to process

  , stateOffset :: {-# UNPACK #-} !Int

    -- ^ Number of processed tokens so far

  , statePosState :: PosState s

    -- ^ State that is used for line\/column calculation
  }
```

### ``

``:

```haskell
```

### ``

``:

```haskell
```

























## `modern-uri`

URIs, per @RFC 3986@.

### module `Text.URI`

#### `URI.`

```haskell

data URI = URI

  { uriScheme :: Maybe (RText 'Scheme)

    -- ^ URI scheme, if 'Nothing', then the URI reference is relative

  , uriAuthority :: Either Bool Authority

    -- ^ 'Authority' component in 'Right' or a 'Bool' value in 'Left'
    -- indicating if 'uriPath' path is absolute ('True') or relative
    -- ('False'); if we have an 'Authority' component, then the path is
    -- necessarily absolute, see 'isPathAbsolute'

  , uriPath :: Maybe (Bool, NonEmpty (RText 'PathPiece))

    -- ^ 'Nothing' represents the empty path, while 'Just' contains an
    -- indication 'Bool' whether the path component has a trailing slash,
    -- and the collection of path pieces @'NonEmpty' ('RText' 'PathPiece')@.
    
ParseException is thrown.  , ...
  
  }
```

#### `URI.mkURI`

```haskell
mkURI :: MonadThrow m => Text -> m URI
```

#### `URI.render`

```haskell
render :: URI -> Text
```

#### `URI.parser`

```haskell
parser :: MonadParsec e Text m => m URI
```

#### `ParseException`

`mkURI` throws `ParseException` (via `throwM`).

#### 









































## `dhall`

### module `Dhall.Import`

```haskell

-- | Hash a fully resolved expression

hashExpression :: StandardVersion -> Expr s X -> (Crypto.Hash.Digest SHA256)
hashExpression _standardVersion expression =
    Crypto.Hash.hash (encodeExpression _standardVersion expression)

{-| Convenience utility to hash a fully resolved expression and return the
    base-16 encoded hash with the @sha256:@ prefix

    In other words, the output of this function can be pasted into Dhall
    source code to add an integrity check to an import
-}

hashExpressionToCode :: StandardVersion -> Expr s X -> Text
hashExpressionToCode _standardVersion expr =
    "sha256:" <> Text.pack (show (hashExpression _standardVersion expr))

```














## `raaz`

### Example

```haskell
import qualified "raaz" Raaz.Hash as Hash
import           "raaz" Raaz.Hash ()


```

### module ``

```haskell
-- | Compute the hash of file.

hashFile :: ( Hash h, Recommendation h)
         => FilePath                    -- ^ File to be hashed
         -> IO h

hashFile fileName = withBinaryFile fileName ReadMode hashSource
```















## `cryptohash-sha256`

> `cryptohash-sha256` is a drop-in cryptohash replacement (of `cryptohash` a.k.a. `cryptonite`) for clients that only need SHA-256.

> A practical incremental and one-pass, pure API to
> the [SHA-256 cryptographic hash algorithm](https://en.wikipedia.org/wiki/SHA-2) according
> to [FIPS 180-4](http://dx.doi.org/10.6028/NIST.FIPS.180-4)
> with performance close to the fastest implementations available in other languages.
> .
> The core SHA-256 algorithm is implemented in C and is thus expected
> to be as fast as the standard [sha256sum(1) tool](https://linux.die.net/man/1/sha256sum);
> for instance, on an /Intel Core i7-3770/ at 3.40GHz this implementation can
> compute a SHA-256 hash over 230 MiB of data in under one second.
> (If, instead, you require a pure Haskell implementation and performance is secondary, please refer to the [SHA package](https://hackage.haskell.org/package/SHA).)
> .
> 
> .
> Additionally, this package provides support for
> .
> - HMAC-SHA-256: SHA-256-based [Hashed Message Authentication Codes](https://en.wikipedia.org/wiki/HMAC) (HMAC)
> - HKDF-SHA-256: [HMAC-SHA-256-based Key Derivation Function](https://en.wikipedia.org/wiki/HKDF) (HKDF)
> .
> conforming to [RFC6234](https://tools.ietf.org/html/rfc6234), [RFC4231](https://tools.ietf.org/html/rfc4231), [RFC5869](https://tools.ietf.org/html/rfc5869), et al..
> .
 
### Example

```haskell
import qualified "cryptohash-sha256" Crypto.Hash.SHA256 as SHA256
import           "cryptohash-sha256" Crypto.Hash.SHA256 ()


```

### module `Crypto.Hash.SHA256`

```haskell
    -- * Incremental API
    --
    -- | This API is based on 4 different functions, similar to the
    -- lowlevel operations of a typical hash:
    --
    --  - 'init': create a new hash context
    --  - 'update': update non-destructively a new hash context with a strict bytestring
    --  - 'updates': same as update, except that it takes a list of strict bytestrings
    --  - 'finalize': finalize the context and returns a digest bytestring.
    --
    -- all those operations are completely pure, and instead of
    -- changing the context as usual in others language, it
    -- re-allocates a new context each time.
    --
    -- Example:
    --
    -- > import qualified Data.ByteString
    -- > import qualified Crypto.Hash.SHA256 as SHA256
    -- >
    -- > main = print digest
    -- >   where
    -- >     digest = SHA256.finalize ctx
    -- >     ctx    = foldl SHA256.update ctx0 (map Data.ByteString.pack [ [1,2,3], [4,5,6] ])
    -- >     ctx0   = SHA256.init

      Ctx(..)
    , init     -- :: Ctx
    , update   -- :: Ctx -> ByteString -> Ctx
    , updates  -- :: Ctx -> [ByteString] -> Ctx
    , finalize -- :: Ctx -> ByteString
    , finalizeAndLength -- :: Ctx -> (ByteString,Word64)

    -- * Single Pass API
    --
    -- | This API use the incremental API under the hood to provide
    -- the common all-in-one operations to create digests out of a
    -- 'ByteString' and lazy 'L.ByteString'.
    --
    --  - 'hash': create a digest ('init' + 'update' + 'finalize') from a strict 'ByteString'
    --  - 'hashlazy': create a digest ('init' + 'update' + 'finalize') from a lazy 'L.ByteString'
    --  - 'hashlazyAndLength': create a digest ('init' + 'update' + 'finalizeAndLength') from a lazy 'L.ByteString'
    --
    -- Example:
    --
    -- > import qualified Data.ByteString
    -- > import qualified Crypto.Hash.SHA256 as SHA256
    -- >
    -- > main = print $ SHA256.hash (Data.ByteString.pack [0..255])
    --
    -- __NOTE__: The returned digest is a binary 'ByteString'. For
    -- converting to a base16/hex encoded digest the
    -- <https://hackage.haskell.org/package/base16-bytestring base16-bytestring>
    -- package is recommended.

    , hash     -- :: ByteString -> ByteString
    , hashlazy -- :: L.ByteString -> ByteString
    , hashlazyAndLength -- :: L.ByteString -> (ByteString,Int64)

    -- ** HMAC-SHA-256
    --
    -- | <https://tools.ietf.org/html/rfc2104 RFC2104>-compatible
    -- <https://en.wikipedia.org/wiki/HMAC HMAC>-SHA-256 digests

    , hmac     -- :: ByteString -> ByteString -> ByteString
    , hmaclazy -- :: ByteString -> L.ByteString -> ByteString
    , hmaclazyAndLength -- :: ByteString -> L.ByteString -> (ByteString,Word64)

    -- ** HKDF-SHA-256
    --
    -- | <https://tools.ietf.org/html/rfc5869 RFC5869>-compatible
    -- <https://en.wikipedia.org/wiki/HKDF HKDF>-SHA-256 key derivation function

    , hkdf
    )
```

### 





























## `cryptonite`

> A repository of cryptographic primitives:
> 
> * Symmetric ciphers: AES, DES, 3DES, CAST5, Blowfish, Twofish, Camellia, RC4, Salsa, XSalsa, ChaCha.
> 
> * Hash: SHA1, SHA2, SHA3, SHAKE, MD2, MD4, MD5, Keccak, Skein, Ripemd, Tiger, Whirlpool, Blake2
> 
> * MAC: HMAC, Poly1305
> 
> * Asymmetric crypto: DSA, RSA, DH, ECDH, ECDSA, ECC, Curve25519, Curve448, Ed25519, Ed448
> 
> * Key Derivation Function: PBKDF2, Scrypt, HKDF, Argon2
> 
> * Cryptographic Random generation: System Entropy, Deterministic Random Generator
> 
> * Data related: Anti-Forensic Information Splitter (AFIS)

### module `Crypto.Hash`

```haskell
import qualified "cryptonite" Crypto.Hash as Hash
import           "cryptonite" Crypto.Hash (SHA256)


```













## `base16-bytestring`

> Fast base16 (hex) encoding and decoding for ByteStrings

### Example

```haskell
>>> import qualified "base16-bytestring" Data.ByteString.Base16.Lazy as Base16
>>> import qualified "bytestring"        Data.ByteString.Lazy as ByteString

>>> :set -XOverloadedStrings 

>>> (ByteString.length (Base16.encode "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
```

### module `Data.ByteString.Base16.Lazy`

```haskell
-- | Encode a string into base16 form.  The result will always be a
-- multiple of 2 bytes in length.
--
-- Example:
--
-- > encode "foo"  == "666f6f"

encode :: ByteString -> ByteString
encode (Chunk c cs) = Chunk (B16.encode c) (encode cs)
encode Empty        = Empty
```














## `generic-lens`

### module `Data.Generics.Product`

`getField`:

```haskell
getField :: forall f a s. HasField' f s a => s -> a

>>> getField @"age" human
50
```

e.g.

```haskell
>>> :set -XTypeApplications
>>> :set -XDataKinds
>>> :set -XDeriveGeneric
>>> :set -XGADTs
>>> :set -XFlexibleContexts

>>> import GHC.Generics
>>> :m +Data.Generics.Internal.VL.Lens
>>> :m +Data.Function

>>> :{
data Human a
  = Human
    { name    :: String
    , age     :: Int
    , address :: String
    , other   :: a
    }
  | HumanNoAddress
    { name    :: String
    , age     :: Int
    , other   :: a
    }
  deriving (Generic, Show)
human :: Human Bool
human = Human { name = "Tunyasz", age = 50, address = "London", other = False }
:}
```











## ``

### module ``

```haskell
```










## Installation

### Static-Linking

by default:

```sh
$ which skeletor-haskell 

/home/sboo/.cabal/bin/skeletor-haskell
```

```sh
$ ldd `which skeletor-haskell`

	linux-vdso.so.1                                    =>                                                    (0x00007ffcdc17d000)
	libm.so.6                                          => /nix/store/*-glibc-2.27/lib/libm.so.6              (0x00007fcb7f8f4000)
	libz.so.1                                          => /nix/store/*-user-environment/lib/libz.so.1        (0x00007fcb7f6d8000)
	libncursesw.so.6                                   => /nix/store/*-user-environment/lib/libncursesw.so.6 (0x00007fcb7f469000)
	libpthread.so.0                                    => /nix/store/*-glibc-2.27/lib/libpthread.so.0        (0x00007fcb7f24a000)
	librt.so.1                                         => /nix/store/*-glibc-2.27/lib/librt.so.1             (0x00007fcb7f042000)
	libutil.so.1                                       => /nix/store/*-glibc-2.27/lib/libutil.so.1           (0x00007fcb7ee3f000)
	libdl.so.2                                         => /nix/store/*-glibc-2.27/lib/libdl.so.2             (0x00007fcb7ec3b000)
	libgmp.so.10                                       => /nix/store/*-gmp-6.1.2/lib/libgmp.so.10            (0x00007fcb7e9a7000)
	libc.so.6                                          => /nix/store/*-glibc-2.27/lib/libc.so.6              (0x00007fcb7e5f3000)
	/nix/store/*-glibc-2.27/lib/ld-linux-x86-64.so.2   => /lib64/ld-linux-x86-64.so.2                        (0x00007fcb7fc89000)
```

with `ld-options: -static`:

```sh
$ cabal new-build --flags="+static" "skeletor:exe:skeletor-haskell"

...
Building executable 'skeletor-haskell' for skeletor-0.0.0..
Linking /home/sboo/haskell/skeletor/dist-newstyle/build/x86_64-linux/ghc-8.6.3/skeletor-0.0.0/x/skeletor-haskell/build/skeletor-haskell/skeletor-haskell ...

ld: cannot find -lm
ld: cannot find -ltinfo
ld: cannot find -lpthread
ld: cannot find -lrt
ld: cannot find -lutil
ld: cannot find -ldl
ld: cannot find -lgmp
ld: cannot find -lm
ld: cannot find -lrt
ld: cannot find -ldl
ld: cannot find -lc

collect2: error: ld returned 1 exit status
`cc' failed in phase `Linker'. (Exit code: 1)
```

with `./cabal-static.project`:

```sh
$ cabal  new-build  "--project-file=./cabal-static.project"  "skeletor:exe:skeletor-haskell"

/nix/store/*-binutils-2.30/bin/ld: /nix/store/*-gcc-7.3.0/lib/gcc/x86_64-unknown-linux-gnu/7.3.0/crtbeginT.o: relocation R_X86_64_32 against hidden symbol `__TMC_END__' can not be used when making a shared object

/nix/store/*-binutils-2.30/bin/ld: final link failed: Nonrepresentable section on output

...
```

with `nh2 / static-haskell-nix`:


```sh
$ 


```
















## Nix & nixpkgs

### `writeTextFile`

<https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/trivial-builders.nix>

```nix
/* Writes a text file to the nix store.
 * The contents of text is added to the file in the store.
 *
 * Examples:
 * # Writes my-file to /nix/store/<store path>
 * writeTextFile "my-file"
 *   ''
 *   Contents of File
 *   '';
 *
 * # Writes executable my-file to /nix/store/<store path>/bin/my-file
 * writeTextFile "my-file"
 *   ''
 *   Contents of File
 *   ''
 *   true
 *   "/bin/my-file";
 *   true
 */

writeTextFile =
  { name # the name of the derivation
  , text
  , executable ? false # run chmod +x ?
  , destination ? ""   # relative path appended to $out eg "/bin/foo"
  , checkPhase ? ""    # syntax checks, e.g. for scripts
  }:

  runCommand name
    { inherit text executable;
      passAsFile = [ "text" ];
      # Pointless to do this on a remote machine.
      preferLocalBuild = true;
      allowSubstitutes = false;
    }
    ''
      n=$out${destination}
      mkdir -p "$(dirname "$n")"
      if [ -e "$textPath" ]; then
        mv "$textPath" "$n"
      else
        echo -n "$text" > "$n"
      fi
      ${checkPhase}
      (test -n "$executable" && chmod +x "$n") || true
    '';
```

### 

















