{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.FFT
  ( FFT (..),
  )
where

-- Add list of exports

import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.Complex
import GHC.Generics
import Generic.Scan
import Generic.Size (Size (..))
import Generic.Types

-- O(NlogN)
-- The FFT algorithm decomposes the DFT into log2N stages,
-- each of which consists of N/2 computations

class FFT f where

  -- Associated type family
  type FFO f :: * -> *

  fft :: RealFloat a => f (Complex a) -> FFO f (Complex a)

  default fft ::
    -- Add the constraint later
    (Generic1 f, Generic1 (FFO f), FFT (Rep1 f), FFO (Rep1 f) ~ Rep1 (FFO f), RealFloat a) =>
    f (Complex a) ->
    FFO f (Complex a)
  fft = to1 . fft . from1

-- transpose = sequenceA

-- These are functors
instance FFT U1 where

  type FFO U1 = U1

  fft U1 = U1

instance FFT Par1 where

  type FFO Par1 = Par1

  fft (Par1 a) = Par1 a

-- We are building FFTs of perfect binary trees
instance FFT Pair where

  type FFO Pair = Pair

  fft (a :# b) = (a + b) :# (a - b)

instance
  ( LScan f,
    LScan g,
    Traversable f,
    Traversable g,
    FFT f,
    FFT g,
    Applicative f,
    Applicative g,
    LScan (FFO g),
    Size (FFO g),
    Traversable (FFO g),
    FFT (FFO g),
    Applicative (FFO g),
    Applicative (FFO f),
    Size f,
    Size g
  ) =>
  FFT (g :.: f)
  where

  type FFO (g :.: f) = FFO f :.: FFO g

  fft = Comp1 . ffts' . sequenceA . twiddle . ffts' . unComp1

ffts' ::
  (Traversable f, Traversable g, Applicative f, Applicative g, FFT f, FFT g, RealFloat a, Applicative (FFO g)) =>
  g (f (Complex a)) ->
  FFO g (f (Complex a))
ffts' = traverse fft . sequenceA

twiddle :: (RealFloat a, LScan f, LScan g, Applicative f, Applicative g, Size f, Size g, Traversable g) => g (f (Complex a)) -> g (f (Complex a))
twiddle = (liftA2 . liftA2) (*) omegas

omegas ::
  forall a f g.
  (RealFloat a, LScan f, LScan g, Applicative f, Applicative g, Size f, Size g, Traversable g) =>
  g (f (Complex a))
omegas = fga
  where
    ga :: g (Complex a)
    ga = fst $ powers (exp (0 :+ negate (2 * pi / fromIntegral (size (Comp1 fga)))))
    fga :: g (f (Complex a))
    fga = fst . powers <$> ga
-- deriving instance FFT (RVec 'Z)
