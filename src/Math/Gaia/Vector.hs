{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Math.Gaia.Vector
    ( Vector(..)
    )
    where

import qualified Protolude as P
import Protolude (($), (<$>), (<*>))
import Math.Gaia
import Math.Gaia.Bool
import Data.Vector.Unboxed

newtype AddVector a = AddVector { unvecAdd :: Vector a }
newtype MulVector a = MulVector { unvecMul :: Vector a }

instance (Distributive a) =>
    Magma (AddVector a) where
    AddVector a `mul` AddVector b = AddVector (a + b)

instance (Distributive a) =>
    Magma (MulVector a) where
    MulVector a `mul` MulVector b = MulVector (a * b)

instance (Distributive a) =>
    Associative (AddVector a)
instance (Distributive a) =>
    Associative (MulVector a)

instance (Distributive a) =>
    Commutative (AddVector a)
instance (Distributive a) =>
    Commutative (MulVector a)

instance (Distributive a) =>
    Unital (AddVector a) where
    unit = AddVector zero

instance (Distributive a) =>
    Unital (MulVector a) where
    unit = MulVector one

instance (Distributive a) =>
    Homomorphic (AddVector a) (AddVector a) where
    hom x = x

instance (Lattice a, Distributive a, Invertible a, Unbox a, P.Num a) =>
    Invertible (AddVector a) where
  inv (AddVector a) = AddVector $ map inv a

instance (Lattice a, Distributive a, Invertible a, Unbox a, P.Num a) =>
    Invertible (MulVector a) where
  inv (MulVector a) = MulVector $ map inv a

instance (
    Distributive a
    ) =>
    Distributive (Vector a) where
    type Add (Vector a) = AddVector a
    type Mul (Vector a) = MulVector a

newtype InfVector a = InfVector (Vector a)
newtype SupVector a = SupVector (Vector a)

instance (P.Ord a, Unbox a) => Magma (InfVector a) where
  InfVector a `mul` InfVector b = InfVector (if a P.<= b then a else b)

instance (P.Ord a, Unbox a) => Magma (SupVector a) where
  SupVector a `mul` SupVector b = SupVector (if a P.>= b then a else b)

instance (P.Ord a, Unbox a) => Associative (InfVector a)
instance (P.Ord a, Unbox a) => Associative (SupVector a)

instance (P.Ord a, Unbox a) => Commutative (SupVector a)
instance (P.Ord a, Unbox a) => Commutative (InfVector a)

instance (P.Ord a, Unbox a) => Idempotent (SupVector a)
instance (P.Ord a, Unbox a) => Idempotent (InfVector a)

instance (P.Ord a, Unbox a, Homomorphic (Inf a) (Sup a), Unbox a, Lattice a) => Homomorphic (SupVector a) (InfVector a) where hom (SupVector a) = InfVector (map negate a)
instance (P.Ord a, Unbox a, Homomorphic (Inf a) (Sup a), Unbox a, Lattice a) => Homomorphic (InfVector a) (SupVector a) where hom (InfVector a) = SupVector (map negate a)

instance (P.Ord a, Unbox a) => POrd (Vector a) where
  pcompare n m = if n P.> m then PGT else if n P.== m then PEQ else PLT

instance (P.Ord a, Unbox a) => Lattice (Vector a) where
  type Inf (Vector a) = InfVector a
  type Sup (Vector a) = SupVector a
