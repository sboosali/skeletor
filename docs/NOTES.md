# Notes

(N.B. this is an incoherent reference document for the library author).

## Templates

e.g.


```haskell
>>> :set -XOverloadedStrings -XOverloadedLists
>>> sedTemplate (TemplateBinding [ ("", ""), ("", "") ]) (SrcDst { input  = "", output = "" })
```

## Usage

e.g.


```haskell
sedTemplate

  (TemplateBinding [ ("", "")
                   , ("", "")
                   ])

  SrcDst { input  = ""
         , output = ""
         }
```

## The Filesystem




## APIs

```haskell
{-

--

instance (NFData k, NFData v) => NFData (HashMap k v)

--

readFile :: FilePath -> IO ByteString

writeFile :: FilePath -> ByteString -> IO ()

--

readFile :: FilePath -> IO Text

The readFile function reads a file and returns the contents of the file as a string. The entire file is read strictly, as with getContents.

writeFile :: FilePath -> Text -> IO ()

Write a string to a file. The file is truncated to zero length before writing begins.

--

  class Hashable a where
    hashWithSalt :: Int -> a -> Int 
    infixl 0

The class of types that can be converted to a hash value.

Minimal implementation: hashWithSalt.

Return a hash value for the argument, using the given salt.

--

18,446,744,073,709,551,615 ≡ 2^64 − 1

--

lookupEnv :: String -> IO (Maybe String)

  Return the value of the environment variable var, or Nothing if there is no such value.

--

splitOn

  :: Text	
     -- ^ String to split on. If this string is empty, an error will occur.

  -> Text	
     -- ^ Input text.

  -> [Text]	 
     -- ^ O(m+n) Break a Text into pieces separated by the first Text argument (which cannot be empty), consuming the delimiter. An empty delimiter is invalid, and will cause an error to be raised.

Examples:

    >>> splitOn "\r\n" "a\r\nb\r\nd\r\ne"
    ["a","b","d","e"]

    >>> splitOn "aaa"  "aaaXaaaXaaaXaaa"
    ["","X","X","X",""]

    >>> splitOn "x"    "x"
    ["",""]

Laws

    intercalate s . splitOn s         == id
    splitOn (singleton c)             == split (==c)

--

Cabal-2.4.1.0:Distribution.System.OS

  These are the known OS names: 
    Linux, Windows, OSX ,FreeBSD, OpenBSD, NetBSD, DragonFly ,Solaris, AIX, HPUX, IRIX ,HaLVM ,Hurd ,IOS, Android,Ghcjs

--

setEnv :: String -> String -> IO ()

  setEnv name value sets the specified environment variable to value.

  Throws IOException if name is the empty string or contains an equals sign.

--

getEnvironment :: IO [(String, String)]

  getEnvironment retrieves the entire environment as a list of (key,value) pairs.
  
  If an environment entry does not contain an '=' character, the key is the whole entry and the value is the empty string.

--

data FileType = BlockDevice
              | CharacterDevice
              | NamedPipe
              | RegularFile
              | Directory
              | SymbolicLink
              | Socket
              | Unknown

--


--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

-}
```

