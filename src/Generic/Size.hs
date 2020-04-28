{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Generic.Size where

import GHC.Generics

class Functor f => Size f where

  size :: f a -> Word

  default size :: (Generic1 f, Size (Rep1 f)) => f a -> Word
  size = size . from1

instance Size V1 where size = const 0

instance Size U1 where size = const 1

instance Size Par1 where size = const 1

instance (Size f, Size g) => Size (f :+: g) where
  size (L1 fa) = size fa
  size (R1 ga) = size ga

instance (Size f, Size g) => Size (f :*: g) where
  size (fa :*: ga) = size fa + size ga

instance (Size f, Size g, Traversable f, Applicative g) => Size (f :.: g) where
  size (unComp1 -> fga) = size fga * size gfa
    where
      gfa = sequenceA fga
