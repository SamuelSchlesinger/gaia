module Gaia.Abstract
  ( Hom(..)
  , Functor(..)
  , Applicative(..)
  , Monad(..)
  , Foldable(..)
  , Traversable(..), mapM
  , Then(..)
  , Logic
  , Equivalence(..)
  , Propositional(..)
  , Conditional(..)
  , Type
  , Constraint
  , module Data.Coerce
  , Dict(..)
  , (:-)(..)
  , Magma(..)
  , Semigroup(..), (<>)
  , Commutative(..)
  , Action(..)
  , Invertible(..)
  , Neutral(..)
  , Monoid(..), (++), empty
  , Group(..), identity
  , Idempotent(..)
  , Band(..)
  , Semilattice(..)
  , Lattice, Join, Meet, (/\), (\/)
  , Distributive, Multiplication, Addition, (+), (*)
  , Semiring(..)
  , Ring(..)
  , Division(..)
  , Field(..)
  , Module(..)
  , Semimodule(..)
  , Premodule(..), (.*)
  ) where

import Data.Kind
import Data.Coerce

-- | Hom is the "homset constructor" of some implicit
--   category.
class Hom (hom :: i -> i -> Type) where
  type hom >- (obj :: i) :: Constraint
  id :: hom >- a => hom a a
  (.) :: hom b c -> hom a b -> hom a c
  dom :: hom a b -> Dict (hom >- a)
  cod :: hom a b -> Dict (hom >- b)

-- | Functor
--
-- A Functor between two categories is the natural way
-- to think about morphisms between categories themselves.
--
-- map (g . f) = map f . map g
class (
    Hom (Dom f)
  , Hom (Cod f)
  ) => Functor f where
  type Dom f :: i -> i -> Type
  type Cod f :: j -> j -> Type
  map :: Dom f a b -> Cod f (f a) (f b)

class (
    Functor f
  , Dom f ~ Cod f 
  ) => Endofunctor (f :: i -> i) where
  type Realm f :: i -> i -> Type

instance (
    Functor f
  , Dom f ~ Cod f
  ) => Endofunctor f where
  type Realm f = Dom f

class (
    Endofunctor f
  ) => Applicative f where
  pure :: Realm f a (f a)
  (<*>) :: f (Realm f a b) -> Realm f (f a) (f b)

class (
    Endofunctor f
  ) => Monad f where
  return :: Realm f a (f a)
  bind :: Realm f a (f b) -> Realm f (f a) (f b)

class Endofunctor f => Foldable f where
  fold :: Monoid m => Realm f (f m) m
  foldMap :: Monoid m => Realm f a m -> Realm f (f a) m
  foldr :: Realm f a (Realm f b b) -> Realm f b (Realm f (f a) b)
  foldr' :: Realm f a (Realm f b b) -> Realm f b (Realm f (f a) b)
  foldl :: Realm f b (Realm f a b) -> Realm f b (Realm f (f a) b)
  foldl' :: Realm f b (Realm f a b) -> Realm f b (Realm f (f a) b)

class Endofunctor f => Traversable f where
  traverse :: Applicative g => Realm f a (g b) -> Realm f (f a) (g (f b))
  sequence :: Applicative g => Realm f (f (g a)) (g (f a))

mapM :: (Monad g, Applicative g, Traversable f) => Realm f a (g b) -> Realm f (f a) (g (f b))
mapM = traverse

class Endofunctor m => Then m where
  (>>) :: Realm f (m a) (Realm f (m b) (m b))

-- | Type Indexed Logic
-- 
-- With every type one uses, there generally is an
-- associated type which is the object one uses to
-- "reason" about this type.
--
-- One fact which I'm really relying on is that, for
-- all of your logics, they are their own logic. This
-- allows me to do things like Conditional. 
type family Logic e :: Type

type instance Logic (a -> e) = a -> Logic e

class Conditional c x where
  ifThenElse :: c -> x -> x -> x

instance Conditional e x => Conditional (a -> e) (a -> x) where
  ifThenElse p a b x = ifThenElse (p x) (a x) (b x)

-- | Equivalence
--
-- These are the objects for which we have a
-- way to query their equality in whatever logic they
-- reside in. Assuming that there is a notion of
-- truth in your logic:
--
-- This must make an equivalence relation in whatever way
-- makes sense for your logical system. Note that you may
-- have a probabalistic system of logic and it might not
-- obey these laws as strictly as you might imagine.
class Equivalence e where
  (==) :: e -> e -> Logic e

instance Equivalence e => Equivalence (a -> e) where
  (f == g) x = f x == g x

-- | Propositional Logic
-- 
-- These are the logics which behave in the way
-- we are used to, obeying these rules:
--
-- @not (and a b) = or   (not a) (not b)@
-- @not (or a b)  = and  (not a) (not b)@
-- @xor a b       = and (not (and a b)) (or a b)@
-- @nand a b      = not (and a b)@
-- @nor a b       = not (or a b)@
-- @and true true = true@
-- @or  true a    = true@
-- @or  a    true = true@
class Logic e ~ e => Propositional e where
  not   :: e -> e
  and   :: e -> e -> e
  and a b = not (nand a b)
  or    :: e -> e -> e
  or a b = not (nor a b)
  xor   :: e -> e -> e
  xor a b = and (not (and a b)) (or a b)
  nand  :: e -> e -> e
  nand a b = not (and a b)
  nor   :: e -> e -> e
  nor a b = not (or a b)
  implies :: e -> e -> e
  implies a b = or (not a) b
  iff :: e -> e -> e
  iff a b = implies a b `and` implies b a
  true  :: e
  false :: e

instance Propositional e => Propositional (a -> e) where
  not f a = not (f a)
  and f g a = and (f a) (g a)
  or f g a = or (f a) (g a)
  xor f g a = xor (f a) (g a)
  nand f g a = nand (f a) (g a)
  nor f g a = nor (f a) (g a)
  implies f g a = implies (f a) (g a)
  iff f g a = iff (f a) (g a)
  true a = true
  false a = true

-- | Explicit Constraint Manipulation
--
-- Haskell's type classes are implemented by essentially mapping
-- each instance declaration at runtime to a dictionary containing
-- the implementation for your type. We can manipulate these dictionaries
-- explicitly using *ConstraintKinds*.
data Dict a where
  Dict :: a => Dict a

newtype a :- b = Sub (a => Dict b)

-- | Binary Operator
--
-- A lawless scoundrel, the magma obeys neither man nor machine
class Magma a where
  mul :: a -> a -> a

instance Magma a => Magma (e -> a) where
  mul f g x = f x `mul` g x

-- | Semigroup
-- 
-- f <> (g <> h) == (f <> g) <> h
--
-- Your type obeying this law is not just for my sanity, it is also for
-- yours, in that generic algorithms will not work as advertised for you if you do 
-- not obey the law.
class Magma a => Semigroup a
(<>) :: Semigroup a => a -> a -> a
(<>) = mul

instance Semigroup a => Semigroup (e -> a)

-- | Magma Action
-- 
-- @act s (act t a) = act (s <> t) a@
class Magma s => Action s a where
  act :: s -> a -> a

instance Action s a => Action (e -> s) (e -> a) where
  act f g x = f x `act` g x

-- | Neutral
-- 
-- A Magma with a neutral element must follow:
--
-- @a `mul` neutral = a@
-- @neutral `mul` a = a@
class Magma a => Neutral a where
  neutral :: a

instance Neutral a => Neutral (e -> a) where
  neutral _ = neutral

-- | Commutative
--
-- A Commutative Magma obeys:
--
-- @a `mul` b == b `mul` a@
class Magma a => Commutative a

instance Commutative a => Commutative (e -> a)

-- | Invertible
--
-- An Invertible Magma has an inverse for every
-- element, such that:
--
-- @a <> inv a == neutral@
-- @inv a <> a == neutral@
class Neutral a => Invertible a where
  inv :: a -> a
  inv a = cancel neutral a
  cancel :: a -> a -> a
  cancel a b = mul a (inv b)

instance Invertible a => Invertible (e -> a) where
  inv f a = inv (f a)
  cancel f g a = cancel (f a) (g a)

-- | Idempotent
--
-- a `mul` a == a
class Magma a => Idempotent a

instance Idempotent a => Idempotent (e -> a)

-- | Monoid
-- 
-- Types which are Associative and Neutral in their Magma are
-- referred to as Monoids.
class    (Semigroup a, Neutral a) => Monoid a
instance (Semigroup a, Neutral a) => Monoid a

{-# INLINE (++) #-}
(++) :: Monoid a => a -> a -> a
(++) = mul

{-# INLINE empty #-}
empty :: Monoid a => a
empty = neutral

-- | Group
--
-- Types which are a Monoid and Invertible in their Magma
-- are referred to as Groups. There is a rich theory of groups which can
-- be read about in many places. Here is one:
-- 
-- Group Theory by J.S. Milne [http://www.jmilne.org/math/CourseNotes/GT.pdf]
class    (Monoid a, Invertible a) => Group a
instance (Monoid a, Invertible a) => Group a
instance Group a => Group (e -> a)

{-# INLINE (&) #-}
(&) :: Group a => a -> a -> a
(&) = mul

{-# INLINE identity #-}
identity :: Group a => a
identity = neutral

class (
    Idempotent s
  , Semigroup s
  ) => Band s

instance (
    Idempotent s
  , Semigroup s
  ) => Band s

class (
    Band s
  , Commutative s
  ) => Semilattice s

instance (
    Band s
  , Commutative s
  ) => Semilattice s

class Semilattice a => Bounded a where
  bound :: a

instance Bounded a => Bounded (x -> a) where
  bound = \_ -> bound

type family Join s
type family Meet s

class (
    Semilattice (Join s)
  , Semilattice (Meet s)
  , Coercible (Join s) s
  , Coercible s (Join s)
  , Coercible (Meet s) s
  , Coercible s (Meet s)
  ) => Lattice s where
  join_ :: s -> s -> s
  join_ a1 a2 = coerce (((coerce a1) :: Join s) <> ((coerce a2) :: Join s))
  meet_ :: s -> s -> s
  meet_ a1 a2 = coerce (((coerce a1) :: Meet s) <> ((coerce a2) :: Meet s))
  top_ :: Bounded (Join s) => s
  top_ = coerce (bound :: Join s)
  bottom_ :: Bounded (Meet s) => s
  bottom_ = coerce (bound :: Meet s)

instance (
    Semilattice (Join s)
  , Semilattice (Meet s)
  , Coercible (Join s) s
  , Coercible s (Join s)
  , Coercible (Meet s) s
  , Coercible s (Meet s)
  ) => Lattice s

(/\) :: Lattice s => s -> s -> s
(/\) = meet_
(\/) :: Lattice s => s -> s -> s
(\/) = join_

top :: (Lattice s, Bounded (Join s)) => s
top = top_
bottom :: (Lattice s, Bounded (Meet s)) => s
bottom = bottom_

type family Addition a
type family Multiplication a


class (
    Magma (Addition a)
  , Magma (Multiplication a)
  , Coercible (Addition a) a
  , Coercible a (Addition a)
  , Coercible (Multiplication a) a
  , Coercible a (Multiplication a)
  ) => Distributive a where
  plus_ :: a -> a -> a
  plus_ a1 a2 = coerce (((coerce a1) :: Addition a) `mul` ((coerce a2) :: Addition a)) :: a
  times_ :: a -> a -> a
  times_ a1 a2 = coerce (((coerce a1) :: Multiplication a) `mul` ((coerce a2) :: Multiplication a)) :: a

instance (
    Magma (Addition a)
  , Magma (Multiplication a)
  , Coercible (Addition a) a
  , Coercible a (Addition a)
  , Coercible (Multiplication a) a
  , Coercible a (Multiplication a)
  ) => Distributive a

(+) :: Distributive a => a -> a -> a
(+) = plus_

(*) :: Distributive a => a -> a -> a
(*) = times_

class (
    Distributive a 
  , Monoid (Addition a) 
  , Monoid (Multiplication a)
  ) => Semiring a

instance (
    Distributive a
  , Monoid (Addition a)
  , Commutative (Addition a)
  , Monoid (Multiplication a)
  ) => Semiring a

class (
    Distributive a
  , Group (Addition a)
  , Commutative (Addition a)
  , Monoid (Multiplication a)
  ) => Ring a where
  (-) :: Ring a => a -> a -> a
  a - b = coerce (((coerce a) :: Addition a ) `cancel` ((coerce b) :: Addition a))

instance (
    Distributive a
  , Group (Addition a)
  , Commutative (Addition a)
  , Monoid (Multiplication a)
  ) => Ring a

-- | A Premodule is simply the Constraints
--   listed below.
class (
    Semiring r
  , Action r m
  , Semigroup m
  , Commutative m
  ) => Premodule r m

(.*) :: Premodule r m => r -> m -> m
r .* m = act r m 

-- | A Semimodule is a Premodule where the action is distributive 
class (
    Premodule r m
  , Monoid m
  , Semiring r
  ) => Semimodule r m

-- A Module is a Semimodule where m is a group and r is a ring
class (
    Ring r
  , Group m
  , Semimodule r m
  ) => Module r m

class (
    Ring a
  , Group (Multiplication a)
  ) => Division a where
  divide_ :: a -> a -> a
  {-# INLINE divide_ #-}
  divide_ a b = coerce (((coerce a) :: (Multiplication a)) `mul` inv ((coerce b) :: (Multiplication a)))

{-# INLINE (/) #-}
(/) :: Division a => a -> a -> a
(/) = divide_

instance (
    Ring a
  , Group (Multiplication a)
  ) => Division a

class (
    Division a
  , Commutative (Multiplication a)
  ) => Field a

instance (
    Division a
  , Commutative (Multiplication a)
  ) => Field a
