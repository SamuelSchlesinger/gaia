{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Math.Gaia.Integer where

import Math.Gaia
import Protolude (Integer(..), ($), (>), (<), (>=), (<=), (==))
import qualified Protolude as P

newtype AddInteger = AddInteger Integer
newtype MulInteger = MulInteger Integer

instance Homomorphic AddInteger MulInteger where
  hom (AddInteger n) = MulInteger (2 P.^ n)

instance Magma AddInteger where
  AddInteger a `mul` AddInteger b = AddInteger (a P.+ b)

instance Magma MulInteger where
  MulInteger a `mul` MulInteger b = MulInteger (a P.* b)

instance Associative AddInteger
instance Associative MulInteger

instance Commutative AddInteger
instance Commutative MulInteger

instance Unital AddInteger where
  unit = AddInteger 0

instance Unital MulInteger where
  unit = MulInteger 1

instance Homomorphic AddInteger AddInteger where
  hom x = x

instance Invertible AddInteger where
  inv (AddInteger a) = AddInteger $ P.negate a

instance Distributive Integer where
  type Add Integer = AddInteger
  type Mul Integer = MulInteger

instance IntegralDomain Integer where
  div = P.div
  mod = P.mod

newtype InfInteger = InfInteger Integer
newtype SupInteger = SupInteger Integer

instance Isomorphic InfInteger SupInteger where
  iso = (\(InfInteger x) -> SupInteger (P.negate x), \(SupInteger x) -> InfInteger (P.negate x))

instance Isomorphic SupInteger InfInteger where
  iso = let (a, b) = iso in (b, a)

instance Magma InfInteger where
  InfInteger a `mul` InfInteger b = InfInteger (if a <= b then a else b)

instance Magma SupInteger where
  SupInteger a `mul` SupInteger b = SupInteger (if a >= b then a else b)

instance Associative InfInteger
instance Associative SupInteger

instance Commutative SupInteger
instance Commutative InfInteger

instance Idempotent SupInteger
instance Idempotent InfInteger

instance Homomorphic SupInteger InfInteger where hom (SupInteger a) = InfInteger (-a)
instance Homomorphic InfInteger SupInteger where hom (InfInteger a) = SupInteger (-a)

instance POrd Integer where
  pcompare n m = if n > m then PGT else if n == m then PEQ else PLT

instance Lattice Integer where
  type Inf Integer = InfInteger
  type Sup Integer = SupInteger
