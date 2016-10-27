{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Math.Gaia.Float where

import Math.Gaia
import Protolude (Float(..), ($), (>), (<), (>=), (<=), (==))
import qualified Protolude as P

newtype AddFloat = AddFloat Float
newtype MulFloat = MulFloat Float

instance Magma AddFloat where
  AddFloat a `mul` AddFloat b = AddFloat (a P.+ b)

instance Magma MulFloat where
  MulFloat a `mul` MulFloat b = MulFloat (a P.* b)

instance Associative AddFloat
instance Associative MulFloat

instance Commutative AddFloat
instance Commutative MulFloat

instance Unital AddFloat where
  unit = AddFloat 0

instance Unital MulFloat where
  unit = MulFloat 1

instance Homomorphic AddFloat AddFloat where
  hom x = x

instance Homomorphic AddFloat MulFloat where
  hom (AddFloat x) = MulFloat (P.exp x)

instance Homomorphic MulFloat AddFloat where
  hom (MulFloat x) = AddFloat (P.log x)

instance Isomorphic AddFloat MulFloat where
  iso = (\(AddFloat x) -> MulFloat (P.exp x), \(MulFloat x) -> AddFloat (P.log x))

instance Isomorphic MulFloat AddFloat where
  iso = let (a, b) = iso in (b, a)

instance Invertible AddFloat where
  inv (AddFloat a) = AddFloat $ P.negate a

instance Distributive Float where
  type Add Float = AddFloat
  type Mul Float = MulFloat

newtype InfFloat = InfFloat Float
newtype SupFloat = SupFloat Float

instance Magma InfFloat where
  InfFloat a `mul` InfFloat b = InfFloat (if a <= b then a else b)

instance Magma SupFloat where
  SupFloat a `mul` SupFloat b = SupFloat (if a >= b then a else b)

instance Associative InfFloat
instance Associative SupFloat

instance Commutative SupFloat
instance Commutative InfFloat

instance Idempotent SupFloat
instance Idempotent InfFloat

instance Homomorphic SupFloat InfFloat where hom (SupFloat a) = InfFloat (-a)
instance Homomorphic InfFloat SupFloat where hom (InfFloat a) = SupFloat (-a)

instance POrd Float where
  pcompare n m = if n > m then PGT else if n == m then PEQ else PLT

instance Lattice Float where
  type Inf Float = InfFloat
  type Sup Float = SupFloat
