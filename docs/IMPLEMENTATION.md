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

```haskell
newtype Alt f a

-- Monoid under <|>.
```

## `containers`

```haskell
foldMapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m 
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

