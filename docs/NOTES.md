# Notes

(N.B. this is an incoherent reference document for the library author).

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

--

-}
```

