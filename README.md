pipes-random
============

[![Build Status](https://travis-ci.org/fosskers/pipes-random.svg?branch=master)](https://travis-ci.org/fosskers/pipes-random)
[![Hackage](https://img.shields.io/hackage/v/pipes-random.svg?style=flat)](https://hackage.haskell.org/package/pipes-random)
[![Stackage Nightly](http://stackage.org/package/pipes-random/badge/nightly)](http://stackage.org/nightly/package/pipes-random)
[![Stackage LTS](http://stackage.org/package/pipes-random/badge/lts)](http://stackage.org/lts/package/pipes-random)

Producers for handling randomness.

### Random Numbers
Use functions like `uniform` and `normal` to generate endless streams of
random numbers of the standard `Num` types. For instance, you could perform
some IO action based on a threshold:

```haskell
{-# LANGUAGE TypeApplications #-}  -- GHC8 only. Provides the @ syntax.

import qualified Pipes.Prelude as P

perhaps :: Effect IO ()
perhaps = uniform @Float >-> P.filter (> 0.1) >-> lift releaseTheHounds
```

### Random Elements from Containers
We expose the functions `finite` and `endless` for randomly Producing
elements from a collection. `finite` will only Produce until each of its
elements have been yielded once. Making a shuffle function then is easy:

```haskell
import           Data.Vector (Vector)
import qualified Pipes.Prelude as P

shuffle :: Vector a -> IO [a]
shuffle = P.toListM . finite
```

`endless` on the other hand will endlessly Produce elements in any order.
Repeats will likely appear long before each element has been yielded once.
You can limit the number of results with `P.take`:

```haskell
import           Data.Vector (Vector)
import qualified Pipes.Prelude as P

twenty :: Vector a -> Producer a IO ()
twenty v = endless v >-> P.take 20
```

For the time being, only `V.Vector`s (all kinds) are supported.
