module Gaia.Orphans.Integer
  (
    Integer
  , FromInteger(..)
  ) where

import GHC.Integer
import Gaia.Abstract
import Gaia.Orphans.Bool

type instance Logic Integer = Bool

type instance Multiplication Integer = Mul
type instance Addition Integer = Add
type instance Join Integer = Max
type instance Meet Integer = Min

newtype Add = Add Integer
newtype Mul = Mul Integer
newtype Min = Min Integer
newtype Max = Max Integer

instance Equivalence Integer where
  (==) = eqInteger

instance Conditional Integer x where
  ifThenElse 0 a b = b
  ifThenElse _ a b = a

class FromInteger a where
  fromInteger :: a -> Integer

instance FromInteger Integer where
  fromInteger a = a

instance Magma Add where
  mul (Add a) (Add b) = Add (plusInteger a b)
instance Magma Mul where
  mul (Mul a) (Mul b) = Mul (timesInteger a b)
instance Magma Min where
  mul (Min a) (Min b) = if leInteger a b then Min a else Min b
instance Magma Max where
  mul (Max a) (Max b) = if geInteger a b then Max a else Max b

instance Neutral Add where
  neutral = Add 0
instance Neutral Mul where
  neutral = Mul 1

instance Idempotent Min
instance Idempotent Max


instance Commutative Add
instance Commutative Mul
instance Commutative Min
instance Commutative Max

instance Semigroup Add
instance Semigroup Mul
instance Semigroup Max
instance Semigroup Min

instance Invertible Add where
  inv (Add a) = Add (negateInteger a)
  (Add a) `cancel` (Add b) = Add (minusInteger a b)
