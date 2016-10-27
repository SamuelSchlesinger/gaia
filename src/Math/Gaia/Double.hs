{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Math.Gaia.Double where

import Math.Gaia
import Protolude (Double(..), ($), (>), (<), (>=), (<=), (==))
import qualified Protolude as P

newtype AddDouble = AddDouble Double
newtype MulDouble = MulDouble Double

instance Magma AddDouble where
  AddDouble a `mul` AddDouble b = AddDouble (a P.+ b)

instance Magma MulDouble where
  MulDouble a `mul` MulDouble b = MulDouble (a P.* b)

instance Associative AddDouble
instance Associative MulDouble

instance Commutative AddDouble
instance Commutative MulDouble

instance Unital AddDouble where
  unit = AddDouble 0

instance Unital MulDouble where
  unit = MulDouble 1

instance Homomorphic AddDouble AddDouble where
  hom x = x

instance Invertible AddDouble where
  inv (AddDouble a) = AddDouble $ P.negate a

instance Distributive Double where
  type Add Double = AddDouble
  type Mul Double = MulDouble

newtype InfDouble = InfDouble Double
newtype SupDouble = SupDouble Double

instance Magma InfDouble where
  InfDouble a `mul` InfDouble b = InfDouble (if a <= b then a else b)

instance Magma SupDouble where
  SupDouble a `mul` SupDouble b = SupDouble (if a >= b then a else b)

instance Associative InfDouble
instance Associative SupDouble

instance Commutative SupDouble
instance Commutative InfDouble

instance Idempotent SupDouble
instance Idempotent InfDouble

instance Homomorphic SupDouble InfDouble where hom (SupDouble a) = InfDouble (-a)
instance Homomorphic InfDouble SupDouble where hom (InfDouble a) = SupDouble (-a)

instance POrd Double where
  pcompare n m = if n > m then PGT else if n == m then PEQ else PLT

instance Lattice Double where
  type Inf Double = InfDouble
  type Sup Double = SupDouble
