{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Math.Gaia.Int where

import Math.Gaia
import Protolude (Int(..), ($), (>), (<), (>=), (<=), (==))
import qualified Protolude as P

newtype AddInt = AddInt Int
newtype MulInt = MulInt Int

instance Homomorphic AddInt MulInt where
  hom (AddInt n) = MulInt (2 P.^ n)

instance Magma AddInt where
  AddInt a `mul` AddInt b = AddInt (a P.+ b)

instance Magma MulInt where
  MulInt a `mul` MulInt b = MulInt (a P.* b)

instance Associative AddInt
instance Associative MulInt

instance Commutative AddInt
instance Commutative MulInt

instance Unital AddInt where
  unit = AddInt 0

instance Unital MulInt where
  unit = MulInt 1

instance Homomorphic AddInt AddInt where
  hom x = x

instance Invertible AddInt where
  inv (AddInt a) = AddInt $ P.negate a

instance Distributive Int where
  type Add Int = AddInt
  type Mul Int = MulInt

instance IntegralDomain Int where
  div = P.div
  mod = P.mod

newtype InfInt = InfInt Int
newtype SupInt = SupInt Int

instance Magma InfInt where
  InfInt a `mul` InfInt b = InfInt (if a <= b then a else b)

instance Magma SupInt where
  SupInt a `mul` SupInt b = SupInt (if a >= b then a else b)

instance Associative InfInt
instance Associative SupInt

instance Commutative SupInt
instance Commutative InfInt

instance Idempotent SupInt
instance Idempotent InfInt

instance Homomorphic SupInt InfInt where hom (SupInt a) = InfInt (-a)
instance Homomorphic InfInt SupInt where hom (InfInt a) = SupInt (-a)

instance POrd Int where
  pcompare n m = if n > m then PGT else if n == m then PEQ else PLT

instance Lattice Int where
  type Inf Int = InfInt
  type Sup Int = SupInt
