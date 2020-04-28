{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.Types
  ( Pair (..),
    Nat (..),
    BTree (..),
    TTree (..),
    -- LVec (..),
    RVec (..),
    NTree (..),
    FTree (..),
    TDTree (..),
    BUTree (..),
    RPow (..),
    LPow (..),
    RBin (..),
    LBin (..),
    Bush (..),
  )
where

import Control.DeepSeq -- (NFData, NFData1)
import GHC.Generics
import Test.QuickCheck

-- data (f :+: g) a = L1 (f a) | R1 (g a) -- Sum
-- data (f :*: g) a = f a * g a           -- Product
-- newtype (f :.: g) a = Comp1 (f (g a))  -- Composition
-- data V1 a                              -- Void
-- newtype U1 a = U1                      -- Unit
-- newtype Par1 a = Par1 a                -- Singleton

-----------------------
-- Lists
-----------------------

-- | Cons list
-- data List a = Nil | Cons a (List a)

-- | Right list, as they grow rightward
-- data RList a = RNil | a :<: RList a

-- | Left list, as they grow leftward
-- data LList a = LNil | LList a :>: a
--   deriving (Generic)
--
-- instance Generic1 RList where
--
--   type Rep1 RList = U1 :+: (Par1 :*: RList)
--
--   from1 RNil = L1 U1
--   from1 (a :<: as) = R1 (Par1 a :*: as)
--
--   to1 (L1 U1) = RNil
--   to1 (R1 (Par1 a :*: as)) = a :<: as

------------------------
-- Trees
------------------------
-- After lists, trees are perhaps the most commonly used data structure in
-- functional programming. Moreover, in contrast with lists, the symmetry
-- possible with trees naturally leads to parallel-friendly algorithms. Also
-- unlike lists, there are quite a few varieties of trees
data BTree a = BLeaf a | BBranch (BTree a) (BTree a)
  deriving (Functor, Foldable, Traversable)

data TTree a = TLeaf a | TBranch (TTree a) (TTree a) (TTree a)

instance Functor TTree where
  fmap h (TLeaf a) = TLeaf (h a)
  fmap h (TBranch t1 t2 t3) = TBranch (fmap h t1) (fmap h t2) (fmap h t3)

instance Foldable TTree where
  foldMap h (TLeaf a) = h a
  foldMap h (TBranch t1 t2 t3) = foldMap h t1 <> foldMap h t2 <> foldMap h t3

instance Traversable TTree where
  traverse h (TLeaf a) = fmap TLeaf (h a)
  traverse h (TBranch t1 t2 t3) = TBranch <$> traverse h t1 <*> traverse h t2 <*> traverse h t3

--

-- data type of Peano numbers (constructed via zero and successor)
-- We promote this data type with DataKinds

-- Thanks to promotion (via theDataKindslanguage extension),Natis not only a new
-- data type with value-level constructors Z and S, but also a
-- new kind with type-level constructors Z and S
data Nat = Z | S Nat

-- We need GADTs to encode the type dependency
data RVec (n :: Nat) (a :: *) where
  RNil :: RVec 'Z a
  (:<:) :: a -> RVec n a -> RVec ('S n) a

deriving instance Functor (RVec n)

deriving instance Foldable (RVec n)

deriving instance Traversable (RVec n)

instance Generic1 (RVec 'Z) where

  type Rep1 (RVec 'Z) = U1

  from1 RNil = U1

  to1 U1 = RNil

instance Generic1 (RVec ('S n)) where

  -- RVec n and LVec n are essentially n-ary functor products of Par1
  type Rep1 (RVec ('S n)) = Par1 :*: RVec n

  from1 (a :<: as) = Par1 a :*: as

  to1 (Par1 a :*: as) = a :<: as

instance Arbitrary a => Arbitrary (RVec 'Z a) where
  arbitrary = pure RNil

instance (Arbitrary a, Arbitrary (RVec n a)) => Arbitrary (RVec ('S n) a) where
  arbitrary = (:<:) <$> arbitrary <*> arbitrary

deriving instance NFData1 (RVec 'Z)

-- deriving instance NFData1 (RVec ('S n))

instance NFData1 (RVec ('S n)) where
  liftRnf _ = rwhnf

-- deriving instance GNFData One (RVec n)

-- instance Generic1 (LVec 'Z) where
--
--   type Rep1 (LVec 'Z) = U1
--
--   from1 LNil = U1
--
--   to1 U1 = LNil
--
-- instance Generic1 (LVec ('S n)) where
--
--   type Rep1 (LVec ('S n)) = LVec n :*: Par1
--
--   from1 (as :>: a) = as :*: Par1 a
--
--   to1 (as :*: Par1 a) = as :>: a
--
-- data LVec :: Nat -> * -> * where
--   LNil :: LVec 'Z a
--   (:>:) :: LVec n a -> a -> LVec ('S n) a

-- n-ary leaf trees, polymorphic over n
data NTree n a = NLeaf a | NBranch (RVec n (NTree n a))

-- deriving instance Functor (Tree n)
--
-- deriving instance Foldable (Tree n)
--
-- deriving instance Traversable (Tree n)

instance Functor (NTree n) where
  fmap h (NLeaf a) = NLeaf (h a)
  fmap h (NBranch ts) = NBranch ((fmap . fmap) h ts) -- \f -> fmap (fmap f) ts

instance Foldable (NTree n) where
  foldMap h (NLeaf a) = h a
  foldMap h (NBranch ts) = (foldMap . foldMap) h ts

instance Traversable (NTree n) where
  traverse h (NLeaf a) = NLeaf <$> h a
  traverse h (NBranch ts) = NBranch <$> (traverse . traverse) h ts

-- Notice that these instance definitions rely only on very little about the Vec n functor. Specifically, for each of Functor, Foldable and Traversable, the
-- instance for Tree n needs only the corresponding instance for Vec n. For
-- this reason, we can generalize from Vec n as follows:
data FTree f a = FLeaf a | FBranch (f (FTree f a))

-- The instance definitions for “f-ary” trees (also known as the “free monad”
-- for the functor f) are exactly as with n-ary, except for making the
-- requirements on f explicit:

deriving instance Functor f => Functor (FTree f)

deriving instance Foldable f => Foldable (FTree f)

deriving instance Traversable f => Traversable (FTree f)

-- NTree n ~= FTree (Vec n)

-- instance Functor f => Functor (FTree f) where
--   fmap h (FLeaf a) = FLeaf (h a)
--   fmap h (FBranch ts) = FBranch ((fmap . fmap) h ts)
--
-- instance Foldable f => Foldable (FTree f) where
--   foldMap h (FLeaf a) = h a
--   foldMap h (FBranch ts) = (foldMap . foldMap) h ts
--
-- instance Traversable f => Traversable (FTree f) where
--   traverse h (FLeaf a) = FLeaf <$> h a
--   traverse h (FBranch ts) = FBranch <$> (traverse . traverse) h ts

-- A top-down tree is either a leaf or an f-structure of trees
-- A bottom-up tree is either a leaf or a tree of f-structures
data TDTree f a = TDLeaf a | TDBranch (f (TDTree f a))

-- Bottom-up trees (BTree) are a canonical example of “nested” or “non-regular”
-- data types, requiring polymorphic recursion
data BUTree f a = BULeaf a | BUBranch (BUTree f (f a))

-- Note that RVec n and LVec n are essentially n-ary functor products of
-- Par1. Similarly, RPow f n and LPow f n are n-ary functor compositions of
-- f. Functor product and functor composition are both associative only up to
-- isomorphism. RVec and RPow are right associations. LVec and LPow are
-- left associations. Different associations, though isomorphic, lead to different
-- algorithms.

-- type family RVec n where
--     RVec Z = U1
--     RVec (S n) = Par1 :*: RVec

-- Closed type families to the rescue
-- type family RPow f n where
--   RPow f 'Z = Par1
--   RPow f ('S n) = f :.: RPow f n
--
type family LPow f n where
  LPow f 'Z = Par1
  LPow f ('S n) = LPow f n :.: f

--  Let’s also rename the types TTree and BTree to “RPow” and “LPow
data RPow :: (* -> *) -> Nat -> * -> * where
  L :: a -> RPow f 'Z a
  B :: f (RPow f n a) -> RPow f ('S n) a

instance Arbitrary a => Arbitrary (RBin 'Z a) where
  arbitrary = L <$> arbitrary

instance (Arbitrary a, Arbitrary (RBin n a)) => Arbitrary (RBin ('S n) a) where
  arbitrary = B <$> arbitrary

-- x = B (B (L 1 :# L 2) :# B (L 3 :# L 4))

instance Generic1 (RPow f Z) where

  type Rep1 (RPow f Z) = Par1

  from1 (L a) = Par1 a

  to1 (Par1 a) = L a

instance Generic1 (RPow f n) => Generic1 (RPow f ('S n)) where

  type Rep1 (RPow f ('S n)) = f :.: RPow f n

  from1 (B ts) = Comp1 ts

  to1 (Comp1 ts) = B ts

data Pair a = a :# a deriving (Functor, Foldable, Traversable)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = (:#) <$> arbitrary <*> arbitrary

-- top-down and bottom-up binary trees:
type RBin n = RPow Pair n

type LBin n = LPow Pair n

-- Bushes
-- In contrast to vectors, the tree types above are perfectly balanced, as is
-- helpful in obtaining naturallyparallel algorithms. From another perspective,
-- however, they are quite unbalanced. The functorcomposition operator is used
-- fully left-associated forLPowand fully right-associated forRPow(hence the
-- names).
-- It’s easy to define a composition-balanced type as well:
type family Bush n where
  Bush 'Z = Pair
  Bush ('S n) = Bush n :.: Bush n

-- While each RBin n and LBin n holds 2^n elements, each statically
-- shaped Bush n holds 2^{2^n} elements.
-- Moreover, there’s nothing special
-- about Pair or binary composition here. Either could be replaced or generalized.
