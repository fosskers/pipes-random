{-# LANGUAGE TypeApplications #-}

-- |
-- Module    : Pipes.Random
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <cwoodbury@azavea.com>
--
-- Producers for handling randomness.

module Pipes.Random
  ( -- * Pseudo-random Number Generators
    pool
    -- * Numbers
  , uniform
  , uniformR
  , normal
    -- * Collections
  , finite
  , endless
  ) where

import           Control.Monad (forever)
import           Control.Monad.Primitive (PrimState)
import qualified Data.Vector.Generic as V
import           Pipes
import qualified Pipes.Prelude as P
import qualified System.Random.MWC as R
import qualified System.Random.MWC.Distributions as R

---

-- | Produce a PRNG using system randomness.
pool :: Producer (R.Gen (PrimState IO)) IO ()
pool = lift (R.withSystemRandom @IO (pure . id)) >>= yield

-- | Endlessly produce anything that's `R.Variate` from a uniform distribution.
uniform :: R.Variate v => Producer v IO ()
uniform = for pool $ \g -> forever (lift (R.uniform g) >>= yield)

-- | Endlessly produce anything that's `R.Variate` from a uniform distribution,
-- within some given range of values.
--
-- * For integral types, inclusive range is used.
-- * For floating types, (a,b] is used.
uniformR :: R.Variate v => (v, v) -> Producer v IO ()
uniformR r = for pool $ \g -> forever (lift (R.uniformR r g) >>= yield)

-- | Given a mean and a standard deviation, endlessly produce values from
-- a normal (Gaussian) distribution.
normal :: Double -> Double -> Producer Double IO ()
normal m sd = for pool $ \g -> forever (lift (R.normal m sd g) >>= yield)

-- | Given some `V.Vector`, produce its elements in a random order, once each.
finite :: V.Vector v a => v a -> Producer a IO ()
finite = undefined

-- | Given some `V.Vector`, endlessly produce elements from it.
--
-- > >>> P.toListM $ endless (V.fromList @Data.Vector.Vector ['a'..'z']) >-> P.take 20
-- > "nvecotyjhestgrrlganj"
endless :: V.Vector v a => v a -> Producer a IO ()
endless v | V.null v = pure ()
          | otherwise = uniformR (0, V.length v - 1) >-> P.map (\i -> v V.! i)
