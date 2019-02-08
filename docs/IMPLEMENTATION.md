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

### `Data.Monoid`

```haskell
newtype Alt f a

-- Monoid under <|>.
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

```haskell
takeExtensions :: FilePath -> String

Get all extensions.

>>> takeExtensions "/directory/path.ext" == ".ext"
>>> takeExtensions "file.tar.gz" == ".tar.gz"
```

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

## ``

## ``

## ``

## ``

## ``

