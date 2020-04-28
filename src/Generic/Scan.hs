{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Generic.Scan where

-- TODO: EXPORT API

import Control.Arrow ((***), first)
import Control.Monad.Zip (MonadZip, munzip, mzipWith)
import Data.Bifunctor (bimap)
import Data.Monoid (Product (..), Sum (..))
import Data.Proxy (Proxy (..))
import GHC.Generics

-- Givn a sequence `a_0,...,a_n-1`, the "prefix sum" is a sequence `b_0,...,b_n`
-- such that `b_k=\sum_{0<=i<k}a_i`. More generally, for any associative operation
-- `+`, the "prefix scan" is defined by `b_k=+_{0<=i<k} a_i`, with `b_0` being the
-- identity for `+`.

-- The extra summary value is the fold over the whole input structure.

class Functor f => LScan f where

  lscan :: Monoid a => f a -> (f a, a)

  default lscan :: (Generic1 f, LScan (Rep1 f), Monoid a) => f a -> (f a, a)
  lscan = first to1 . lscan . from1

-- Enabled by EmptyCase
instance LScan V1 where lscan = \case

instance LScan U1 where lscan U1 = (U1, mempty)

instance LScan Par1 where lscan (Par1 a) = (Par1 mempty, a)

instance (LScan f, LScan g) => LScan (f :+: g) where
  lscan (L1 fa) = first L1 (lscan fa)
  lscan (R1 ga) = first R1 (lscan ga)

-- Because we are left-scanning, every prefix offis also a prefix of f×g, so
-- the lscan results for f are also correct results for f×g.
-- The prefixes of g are not
-- prefixes of f×g, however, since eachg-prefix misses all off. The prefixsums,
-- therefore, are lacking the summary (fold) of all off, whichcorresponds to the
-- last output of thelscanresult forf. All we need to do, therefore, is adjust
-- eachgresult by the finalfresult
instance (LScan f, LScan g) => LScan (f :*: g) where
  lscan (fa :*: ga) = (fa' :*: fmap (fx <>) ga', fx <> gx)
    where
      (fa', fx) = lscan fa
      (ga', gx) = lscan ga

-- LVec3 ◦ LVec4 = (LVec 4 x LVec 4) x LVec 4  
instance (LScan g, LScan f, MonadZip g) => LScan (g :.: f) where
  lscan (Comp1 gfa) = (Comp1 (mzipWith adjustl tots' gfa'), tot)
    where
      (gfa', tots) = munzip (fmap lscan gfa)
      (tots', tot) = lscan tots
      adjustl t = fmap (t <>)

-- class Newtype n where
--
--   type O n :: *
--
--   pack :: O n -> n
--
--   unpack :: n -> O n
--
-- lscanNew :: forall n o f. (Newtype n, LScan f, o ~ O n, Monoid n) => Proxy n -> f o -> (f o, o)
-- lscanNew _ = (fmap unpack *** unpack) . lscan . fmap (pack @n)

lproducts :: forall f a. (LScan f, Num a) => f a -> (f a, a)
lproducts = (fmap getProduct *** getProduct) . lscan . fmap Product

lsums :: forall f a. (LScan f, Num a) => f a -> (f a, a)
lsums = (fmap getSum *** getSum) . lscan . fmap Sum

powers :: (LScan f, Applicative f, Num a) => a -> (f a, a)
powers = lproducts . pure

---
evalPoly :: (LScan f, Foldable f, Applicative f, Num a) => f a -> a -> a
evalPoly coeffs x = coeffs `dotp` fst (powers x)

dotp :: (Foldable f, Applicative f, Num a) => f a -> f a -> a
u `dotp` v = sum ((*) <$> u <*> v)
