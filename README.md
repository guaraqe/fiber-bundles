# fiber-bundles

This is a library for representing the structure of a [fiber
bundle](https://en.wikipedia.org/wiki/Fiber_bundle). This is done through a
class:

``` haskell
class FiberBundle a where
  type Base a :: *
  base :: a -> Base a
```

More details can be found on [Hackage](http://hackage.haskell.org/package/fiber-bundles-0.1.0/candidate).
