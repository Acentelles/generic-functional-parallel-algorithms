# Abstract

- Parallel programming has long focused on arrays as the central data type.
- Generic functional programming decomposes data types such as lists and various
  forms of trees into a small set of building blocks: sum, product, composition
  and their associated identities. Definitions over these few fundamental type
  constructions automatically assemble into algorithms for an infinite variety of
  data types.
- This paper presents generic functional formulations for two important clases
  of parallel algorithms: parallel scan (generalized prefix sum) and fast
  Fourier transform (FFT).
- Notably, arrays play no role in these formulations.
- Consequent benefits include a simpler and more compositional style, much more
  use of common algebraic patterns and freedom from possibility of run-time
  indexing errors.
- The functional generic style reveals deep commonality among what otherwise
  appear to be quite different algorithms.


# Introduction

The basic idea of datatype-generic programming in functional languages is to
relate a broad range of types to a small set of basic ones via isomorphism.

Functor building blocks:
```haskell
data (f :+: g) a = L1 (f a) | R1 (g a) -- Sum
data (f :*: g) a = f a * g a           -- Product
newtype (f :.: g) a = Comp1 (f (g a))  -- Composition
data V1 a                              -- Void
newtype U1 a = U1                      -- Unit
newtype Par1 a = Par1 a                -- Singleton
```

To make encodings of data types easy, GHC.Generics comes with a generic deriving
mechanism (enabled by the DeriveGeneric language extension) so that for regular
(not generalized) algebraic data types, one can simply write "data... deriving
Generic" for types of kind *. For type constructors of kind `* -> *`, one derives
Generic1 instead. Instances for non-regular algebraic data types can be defined
explicitly, which amounts to giving a representation functor `Rep1 f` along with
encoding and decoding operations `to1` and `from1`. To define a generic
algorithm, one provides class instances for these primitives and writes a
default definition for each method in terms of `from1` and `to1`.

```haskell
class Generic a where
  type Rep a :: * -> *
  from :: a -> Rep a p
  to   :: Rep a p -> x

class Generic1 f where
    type Rep1 f :: * -> *
    from1 :: f a -> Rep1 f a
    to1 :: Rep1 f a -> f a
```

The effectiveness of generic programming relies on having at our disposal a
variety of data types, each corresponding to a unique composition of the generic
type building blocks. In contrast, parallel algorithms are usually designed and
implemented in terms of the single data type of arrays (or lists in functional
formulations). Frequently, an array is split, each half processed recursively
and independently, and results combined later. Alternatively, adjacent elements
pairs are combined, resulting in an array of half size for further processing.
The essential idea of these two patterns is the natural fold for perfect, binary
leaf trees of two different varieties, but this essence is obscured by implicit
encodings of trees as arrays. The correctness of the algorithm depends on
careful translation of the natural tree algorithm. Mistakes typically hide in
the tedious details of index arithmetic, which will not be caught by a type
checker (unless programmed with dependent types and full correctness proofs),
instead of manifesting at run-time in the form of incorrect results and/or index
out-of-bounds errors.

Note that this array reduction algorithm only works for arrays whose size is a
power of two. This restriction is a dynamic condition rather than part of the
type signature. If we use the essential data type (a perfect, binary leaf tree)
directly rather than via an encoding, it is easy to capture this restriction in
the type system and check it statically.

Array encodings distance programs from the elegant and well-understood laws and
abstraction that motivate programs, justify their correctness, and point to
algorithmic variations that solve related problems or make different
implementation trade-offs.

Even determinacy of an imperative, array-based parallel algorithm can be
difficult to ensure or verify. When the result is an array, as in scans and
FFTs, values are written to indexed locations. In the presence of paralllelism,
determinacy depends on those write indices being distinct, which again is a
subtle, encoding-specific property, unlikely to be verified automatically.

Why arrays are so widely used in parallel algorithms? One would argue that there is
a relatively straightforward mapping from algorithm to efficient implementation
primitives.

But we can write algorithms in an elegant, modular style using a variety of data
types and the standard algebraic abstractions on those data types, such as
Functor, Applicative, Foldable and Traversable and generate very efficient
implementations. Better yet, we can define such algorithms generically.

Contributions:

- Simple specification of an infinite family of parallel algorithms for ach of
  scan and FFT, indexed by data type and composed out of six generic functor
  combinators.

- Demonstration of functor composition as the heart of both scan and FFT.
  Functor composition provides a statically typed alternative to run-time
  factoring of array sizes often used in FFT algorithms.

- A simple duality between the well-known scan algorithms of Sklansky and of
  Ladner and Fished, revealed by the generic decomposition.

- Compositional complexity analysis (work and depth), also based on functor combinators.


# Some useful data types

```haskell
-- Cons lists
data List a = Nil | Cons a (List a)
```

```haskell
-- Right lists
data RList a = RNil | a <| RList a
```

```haskell
-- Left lists
data LList a = LNil | LList a |> a
```

These two types are isomorphic to:

```haskell
type RList ~= U1 :+: (Par1 :*: RList)
type LList ~= U1 :+: (LList :*: Par1)
```

Spelling out the isomorphisms explicitely,

```haskell
instance Generic1 RList where
    type Rep1 RList = U1 + (Par1 :*: RList)
    from RNil = L1 U1
    from (a <| as) = R1 (Par1 a :*: as)
    to (L1 U1) = RNil
    to (R1 (Par1 a :*: as)) = a <| as
```

```haskell
instance Generic1 LList where
    type Rep1 LList = U1 + (LList :*: Par1)
    from LNil = L1 U1
    from (as |> a) = R1 (as :*: Par1 a)
    to (L1 U1) = LNil
    to (R1 (as :*: Par1 a)) = as |> a
```

`RList` and `LList` are isomorphic not only to their underlying representation
functors, but also to each other. Why would we want to distinguish between them?
One reason is that they may capture different intentions.

## Top-down trees

In contrast with lists, the symmetry possible with trees naturally leads to
parallel-friendly algorithms. Also, unlike lists, there are quite a few
varieties of trees.

```haskell
-- Binary leaf tree
data BinTree a = Leaf a | Branch (BinTree a) (BinTree a)
```

```haskell
-- Ternary leaf tree
data Tree a = Leaf a | Branch (Tree a) (Tree a) (Tree a)
```

The repetition present in the data type definition will be mirrored in instance
definitions. For instance, for ternary leaf trees:

```haskell
instance Functor Tree where
    fmap h (Leaf a) = Leaf (h a)
    fmap h (Branch t1 t2 t3) = Branch (fmap h t1) (fmap h t2) (fmap h t3)
```

```haskell
instance Foldable Tree where
    foldMap h (Leaf a) = h a
    foldMap h (Branch t1 t2 t3) = foldMap h t1 <> foldMap h t2 <> foldMap h t3
```

```haskell
instance Traversable Tree where
    traverse h (Leaf a) = fmap Leaf (h a)
    traverse h (Branch t1 t2 t3) = Branch <$> traverse h t1 <*> traverse h t2 <*> traverse h t3
```

Not only we have repetition among each instance definition (the three
occurrences above), we also have repetition among instance for n-ary trees for
different n.

Assume we have a functor of finite lists statically indexed by length as well as
element type:

```haskell
type Vec :: Nat -> * -> *

instance Functor (Vec n) where ...
instance Foldable (Vec n) where ...
instance Traversable (Vec n) where ...
```

Define a single type of n-ary leaf trees, polymorphic over `n`:

```haskell
data Tree n a = Leaf a | Branch (Vec n (Tree n a))
```

```haskell
instance Functor (Tree n) where
    fmap h (Leaf a) = Leaf (h a)
    fmap h (Branch ts) = Branch ((fmap . fmap) h ts)

instance Foldable (Tree n) where
    foldMap h (Leaf a) = h a
    foldMap h (Branch ts) = (foldMap . foldMap) h ts

instance Traversable (Tree n) where
    traverse h (Leaf a) = Leaf <$> h a
    traverse h (Branch ts) = Branch <$> ((traverse . traverse) h ts)
```

Notice that these instance definitions rely only on very little about the `Vec
n` functor. Specifically, for each of Functor, Foldable and Traversable, the
instance for `Tree n` needs only the corresponding instance for `Vec n`. For
this reason, we can generalize from `Vec n` as follows:

```haskell
data Tree f a = Leaf a | Branch (f (Tree f a))
```

The instance definitions for "f-ary" trees (also knwon as the "free monad" for
the functor f) are exactly as with n-ary, except for making the requirements on
f explicit:

```haskell
instance Functor f => Functor (Tree f) where
    fmap h (Leaf a) = Leaf (h a)
    fmap h (Branch ts) = Branch ((fmap . fmap) h ts)

instance Functor f => Foldable (Tree f) where
    foldMap h (Leaf a) = h a
    foldMap h (Branch ts) = (foldMap . foldMap) h ts

instance Functor f => Traversable (Tree f) where
    traverse h (Leaf a) = Leaf <$> h a
    traverse h (Branch ts) = Branch <$> ((traverse . traverse) h ts)
```

This generalization covers "list-ary" (rose) trees and even "tree-ary" trees.
With this functor parametrized tree type, we can reconstruct n-ary trees as
`Tree (Vec n)`.

Just as there are both left- and right-growing trees, f-ary trees come in two
flavors as well. The form above are all "top-down", in the sense that successive
unwrappings of branch nodes reveal subtrees moving from the top downward. There
are also "bottom-up" trees, in which successive branch node unwrappings reveal
the information in the subtrees from the bottom moving upward. In short:

- A top-down tree is either a leaf or an f-structure of trees
- A bottom-up tree is either a leaf or a tree of f-structures

In Haskell,

```haskell
data TTree f a = TLeaf a | TBranch (f (TTree f a))
data BTree f a = BLeaf a | BBranch (BTree f (f a))
```

Bottom-up trees (BTree) are a canocial example of "nested" or "non-regular" data
types, requiring polymorphic recursion.

## Statically Shaped Variations

Some algorithms work only on collections of restricted size. For instance, the
most common parallel scan and FFT algorithms are limited to arrays of size
`2^n`, while the more general (not just binary) Cooley-Tukey FFT algorithms
require composite size, i.e. mn for integers m, n >= 2. In array-based
algorithms, these restrictions can be realized in one of two ways:

- check array sizes dynamically, incurring a performance penalty
- document the restriction, assume the best, and blame the library user for errors

A third option - much less commonly used - is to statically verify the size
restriction at the call size, perharps by using a dependently type language and
providing proofs as part of the call.

A lightweight compromise is to simulate some of the power of dependent types via
type-level encodings of sizes, as with `Nat` for indexing the `Vec` type.

```haskell
data Nat = Z | S Nat
```

Thanks to promotion (via the `DataKinds` language extension), `Nat` is not only
a new data type with value-level constructors, but also a new kind with
type-level constructors `Z` and `S`.

#### GADT Formulation

Now we can define the length-indexed `Vec` type mentioned above. As with lists,
there are right- and left-growing versions:

```haskell
data RVec :: Nat -> * -> * where
    RNil :: RVec Z a
    :<: :: a -> RVec n a -> RVec (S n) a
```

```haskell
data LVec :: Nat -> * -> * where
    LNil :: LVec Z a
    :>: :: LVec n a -> a -> LVec (S n) a
```

Recall that the generic representations of `RList` and `LList` were built out of
sum, unit, identity and product. With static shaping, the sum disappears from
the representation, moving from dynamic to static choice, and each `Generic1`
instance split into two:


```haskell
instance Generic1 (RVec Z) where
    type Rep1 (RVec Z) = U1
    from1 RNil = U1
    to1 U1 = RNil

instance Generic1 (RVec (S n)) where
    type Rep1 (RVec (S n)) = Par1 :*: RVec n
    from1 (a :<: as) = Par1 a :*: as
    to1 (Par1 a :*: as) = a :<: as
```

```haskell
instance Generic1 (LVec Z) where
    type Rep1 (LVec Z) = U1
    from1 LNil = U1
    to1 U1 = LNil

instance Generic1 (LVec (S n)) where
    type Rep1 (LVec (S n)) = LVec n :*: Par1
    from1 (as :>: a) = as :*: Par1 a
    to1 (as :*: Par1 a) = as :>: a
```

For leaf trees, we have a choice between imperfect and perfect trees. A
"perfect" leaf tree is one in which all leaves are at the same depth. Both
imperfect and perfect can be "statically shaped", but we'll use just perfect
trees in this paper, for which a single type-level number signifying the depth
of all leaves. For succinctness, rename `Leaf` and `Branch` to `L` and `B`. We
also rename `TTree` and `BTree` to `RPow` and `LPow`.

```haskell
data RPow :: (* -> *) -> Nat -> * -> * where
    L :: a -> RPow f Z a
    B :: f (RPow f n a) -> RPow f (S n) a
```

```haskell
data LPow :: (* -> *) -> Nat -> * -> * where
    L :: a -> LPow f Z a
    B :: LPow f n (f a) -> LPow f (S n) a
```

```haskell
instance Generic1 (RPow f Z) where
    type Rep1 (RPow f Z) = Par1
    from1 :: (L a) = Par1 a
    to1 :: (Par1 a) = L1 a
```

```haskell
instance Generic1 (RPow f (S n)) where
    type Rep1 (RPow f (S n)) = Par1 :*: RPow f n
    from1 :: B f = Comp1 f
    to1 :: Comp1 f = B f
```

```haskell
instance Generic1 (LPow f Z) where
    type Rep1 (LPow f Z) = Par1
    from1 :: L a = Par1 a
    to1 :: Par1 a = L a
```

```haskell
instance Generic1 (LPow f (S n)) where
    type Rep1 (LPow f (S n)) = LPow f n :*: Par1
    from1 :: B f = Comp1 f
    to1 :: Comp1 f = B f
```

We can then give these statically shaped data types Functor, Foldable and
Traversable instances matching the dynamically shaped versions given above. In
addition, they have Applicative and Monad instances. Since all of these types
are memo tries, their class instances follow homomorphically from the
corresponding instances for functions.

## Type Family Formulation

Note that `RVec n` and `LVec n` are essentially n-ary functor products of
`Par1`. Similarly, `RPow f n` and `LPow f n` are n-ary functor compositions of
f. Functor product and functor composition are both associative only up to
isomorphism. `RVec` and `RPow` are right associations. `LVec` and `LPow` are
left associations. Different associations, though isomorphic, lead to different
algorithms.

Instead of the GADT-based definitions given above for RVec, LVec, RPow and LPow,
we can make the repeated product and repeated composition more apparent by using
closed type families, with instances defined inductively over type-level natural
numbers.

```haskell
type family RVec n where
    RVec Z = U1
    RVec (S n) = Par1 :*: RVec n
```

```haskell
type family RPow f n where
    RPow f Z = Par1
    RPow f (S n) = f :.: RPow f n
```

Although `RPow` and `LPow` work with any functor argument, we will use uniform
pairs in the examples. The uniform `Pair` functor can be defined in a variety of
ways, including `Par1 :*: Par1`, `RVec 2`, `LVec 2` or its own algebraic data
type:
```haskell
data Pair a = a :# a deriving (Functor, Foldable, Traversable)
```

For convenience, define top-down and bottom-up binary trees:
```haskell
type RBin = RPow Pair
type LBin = LPow Pair
```

## Bushes

In contrast to vectors, the tree types above are perfectly balanced, and is
helpful in obtaining naturally parallel algorithms. From another perspective,
however, they are quite unbalanced. The functor composition operator is used
fully left-associated for `LPow` and fully right-associated for `RPow`. It's
easy to define a composition-balanced type as well:
```haskell
type family Bush n where
    Bush Z = Pair
    Bush (S n) = Bush n :.: Bush n
```

While each `RBin n` and `LBin n` holds `2^n` elements, each statically shaped
`Bush n` holds `2^(2^n)` elements.

Our bush type is inspired by an example of nested data types that has a less
regular shape
```haskell
data Bush a = NilB | ConsB a (Bush (Bush a))
```

Bushes are to tree as trees are to vectors in the following sense. Functor
product is associative up to isomorphism. Where `RVec` and `LVec` choose fully
right- or left- associated products, `RBin` and `LBin` form perfectly and
recursively balanced products (being repeated compositions of Pair). Likewise,
functor composition is associative up to isomorphism. Where `RBin` and `LBin`
are fully right and left- associated, `Bush n` forms balanced compositions. Many
other variations are possible, but the `Bush` definition above will suffice for
this paper.

# Parallel Scan

Givn a sequence `a_0,...,a_n-1`, the "prefix sum" is a sequence `b_0,...,b_n`
such that `b_k=\sum_{0<=i<k}a_i`. More generally, for any associative operation
`+`, the "prefix scan" is defined by `b_k=+_{0<=i<k} a_i`, with `b_0` being the
identity for `+`.

Applications for scan:

- Adding multi-precision numbers
- Polynomial evaluation
- Solving recurrences
- Sorting
- Solving tridiagonal linear systems
- Lexical analysis
- Regular expression search
- Labelling components in two dimensional images

Scans may be "prefix" (from the left) or "suffix" (from the right). Scans may be
"exclusive" if `a_k` does not influence `b_k` or "inclusive" if it does.

There is one more output element than input, which is atypical in the literature
on parallel prefix algorithms, perhaps because scans are more often performed in
place. The additional output makes for an elegant generic decomposition.

The standard list prefix scans in Haskell, `scanl` and `scanr`, also yield one
more output element than input, which is possible for lists. For other data
types, such as trees and especially perfect ones, there may not be a natural
place to store the extra value. For a generic scan applying to many different
data types, we can simply form a product, so that scanning maps `f a` to `f a x
a`. The extra summary value is the fold over the whole input structure. We thus
have the following class for left-sannable functors:
```haskell
class Functor f => LScan f where
    lscan :: Monoid a => f a -> f a :*: a
```

When `f` is in `Traversable`, there is a simple and general specification using
operations from the standard Haskell libraries:

```haskell
lscan === swap . mapAccumL (\acc a -> (acc :+: a, acc)) 0
```

Although all of the example types in this paper are indeed in `Traversable`,
using this `lscan` specification as an implementation would result in an
entirely sequential implementation, since data dependencies are linearly
threaded thorugh the whole computation.

Rather than defining `LScan` instances for all of our data types, the idea of
generic programming is to define instances only for the samll set of fundamental
functor combinators and the automatically compose instances for other types via
the generic encodings (derived automatically when possible). To do so, we can
simply provide a default signature and definition for functors with such
encodings:

```haskell
class Functor f => LScan f where
    lscan :: Monoid a => f a -> f a :*: a
    default lscan :: (Generic1 f, LScan (Rep1 f), Monoid a) => f a -> f a :*: a
    lscan = first to1 . lscan . from1
```

Once we define `LScan` instances for our six fundamental combinators, one can
simply write `instance LScan F` for any functor `F` having a `Generic1`
instance. For our statically shaped vector, tree and bush functors, we have a
choice: use the GADT definitions with their manually defined `Generic1`
instances (exploiting the `lscan` default), or use the type family versions
without de need for the encoding (`from1`) and decoding (`to1`) steps.

## Easy instances

Four of the six needed generic `LScan` instances are easily handled:
```haskell
instance LScan V1 where
    lscan = \case -- TODO: What is going on?
```

```haskell
instance LScan U1 where
    lscan U1 = (U1, 0)
```

```haskell
instance LScan Par1 where
    lscan (Par1 a) = (Par1 0, a)
```

```haskell
instance (LScan f, LScan g) => LScan (f :+: g) where
    lscan (L1 fa) = first L1 (lscan fa)
    lscan (R1 ga) = first R1 (lscan ga)
```





