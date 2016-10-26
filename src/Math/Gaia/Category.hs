{-# LANGUAGE
    TypeInType
  , PolyKinds
  , DataKinds
  , ConstraintKinds 
  , TypeFamilies
  , TypeOperators 
  , RankNTypes 
  , FlexibleInstances 
  , FlexibleContexts 
  , NoImplicitPrelude 
  , GADTs 
  , AllowAmbiguousTypes 
  , UndecidableInstances 
  , UndecidableSuperClasses #-}

module Math.Gaia.Category 
  (
    Hom(..)
  , Quiver(..)
  , Category(..)
  , Groupoid(..)
  , Monoidal(..)
  , Arrow(..)
  ) where

import Data.Kind
import Data.Constraint
import Data.Constraint.Forall

-- | Abstraction

data family Hom :: k -> k -> Type

class Vacuous x
instance Vacuous x

class Hom ~ hom => Quiver hom where
  (~>) :: hom i j -> hom j k -> hom i k
  (<~) :: hom j k -> hom i j -> hom i k
  f <~ g = g ~> f
  f ~> g = g <~ f

class Quiver hom => Category hom where
  id :: hom i i

(.) :: Category hom => hom j k -> hom i j -> hom i k
(.) = (<~)

(>>>) :: Category hom => hom i j -> hom j k -> hom i k
(>>>) = (~>)

(<<<) :: Category hom => hom i j -> hom k i -> hom k j 
(<<<) = (.)

class Category hom => Groupoid hom where
  inv :: hom i j -> hom j i

(/) :: Groupoid hom => hom i j -> hom l j -> hom i l
f / g = f ~> inv g

class (Category (Dom f), Category (Cod f)) => Functor (f :: i -> j) where
  type Dom f :: i -> i -> Type
  type Cod f :: j -> j -> Type
  fmap :: Dom f a b -> Cod f (f a) (f b)

class Quiver hom => Monoidal (hom :: k -> k -> Type) where
  type (a :: k) <*> (b :: k) :: k
  type I :: k
  associator :: (hom ((a <*> b) <*> c) (a <*> (b <*> c)), hom (a <*> (b <*> c)) ((a <*> b) <*> c))
  lunitor :: (hom (I <*> a) a, hom a (I <*> a))
  runitor :: (hom (a <*> I) a, hom a (a <*> I))

class Monoidal hom => Arrow hom where
  first :: hom a b -> hom (a <*> x) (b <*> x)
  second :: hom a b -> hom (x <*> a) (x <*> b)
  (***) :: hom a b -> hom x y -> hom (a <*> x) (b <*> y)
  (&&&) :: hom a x -> hom a y -> hom a (x <*> y)

----------------------------------------------------------

-- | Concretion

newtype instance Hom x y 
  = Fun (x -> y)

newtype instance Hom (f :: k -> j) (g :: k -> j) 
  = Nat (forall (x :: k). Hom (f x) (g x))

newtype instance Hom x y
  = Con (x :- y)

instance Quiver (Hom :: j -> j -> Type) => Quiver (Hom :: (k -> j) -> (k -> j) -> Type) where
  Nat f ~> Nat g = Nat (f ~> g)

instance Category (Hom :: j -> j -> Type) => Category (Hom :: (k -> j) -> (k -> j) -> Type) where
  id = Nat id


