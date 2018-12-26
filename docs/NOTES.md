# Notes

(N.B. this is an incoherent reference document for the library author).

## The Filesystem





## APIs

```haskell
{-

--

readFile :: FilePath -> IO ByteString

writeFile :: FilePath -> ByteString -> IO ()

--

readFile :: FilePath -> IO Text

The readFile function reads a file and returns the contents of the file as a string. The entire file is read strictly, as with getContents.

writeFile :: FilePath -> Text -> IO ()

Write a string to a file. The file is truncated to zero length before writing begins.

--

-}
```

