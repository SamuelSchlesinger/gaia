{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Math.Gaia.Rational where

import Math.Gaia
import Protolude (Rational(..), ($), (>), (<), (>=), (<=), (==))
import qualified Protolude as P

newtype AddRational = AddRational Rational
newtype MulRational = MulRational Rational

instance Magma AddRational where
  AddRational a `mul` AddRational b = AddRational (a P.+ b)

instance Magma MulRational where
  MulRational a `mul` MulRational b = MulRational (a P.* b)

instance Associative AddRational
instance Associative MulRational

instance Commutative AddRational
instance Commutative MulRational

instance Unital AddRational where
  unit = AddRational 0

instance Unital MulRational where
  unit = MulRational 1

instance Homomorphic AddRational AddRational where
  hom x = x

instance Invertible AddRational where
  inv (AddRational a) = AddRational $ P.negate a

instance Invertible MulRational where
  inv (MulRational a) = MulRational $ P.recip a

instance Distributive Rational where
  type Add Rational = AddRational
  type Mul Rational = MulRational

newtype InfRational = InfRational Rational
newtype SupRational = SupRational Rational

instance Magma InfRational where
  InfRational a `mul` InfRational b = InfRational (if a <= b then a else b)

instance Magma SupRational where
  SupRational a `mul` SupRational b = SupRational (if a >= b then a else b)

instance Associative InfRational
instance Associative SupRational

instance Commutative SupRational
instance Commutative InfRational

instance Idempotent SupRational
instance Idempotent InfRational

instance Homomorphic SupRational InfRational where hom (SupRational a) = InfRational (-a)
instance Homomorphic InfRational SupRational where hom (InfRational a) = SupRational (-a)

instance POrd Rational where
  pcompare n m = if n > m then PGT else if n == m then PEQ else PLT

instance Lattice Rational where
  type Inf Rational = InfRational
  type Sup Rational = SupRational
