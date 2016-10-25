{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Gaia.Bool
  (
    Bool(..)
  , And(..)
  , Or(..)
  , Xor(..)
  ) where

import qualified Prelude as P
import Prelude (($), Bool(..))
import Math.Gaia

newtype And = And Bool
newtype Or  = Or  Bool
newtype Xor = Xor Bool

instance Magma And where
  And a `mul` And b = And (a P.&& b)
 
instance Magma Or  where
  Or  a `mul` Or  b = Or  (a P.|| b)

instance Magma Xor where
  Xor True `mul` Xor True = Xor False
  Xor False `mul` Xor False = Xor False
  _ `mul` _ = Xor True

instance Associative And
instance Associative Or
instance Associative Xor

instance Commutative And
instance Commutative Or
instance Commutative Xor

instance Idempotent Or
instance Idempotent And

instance Unital Or where unit = Or False
instance Unital And where unit = And True
instance Unital Xor where unit = Xor False

instance Invertible Xor where inv a = a

instance Homomorphic Or And where hom (Or x) = And (P.not x)

instance Homomorphic And Or where hom (And x) = Or (P.not x)

instance POrd Bool where
  pcompare True True = PEQ
  pcompare True False = PGT
  pcompare False True = PLT
  pcompare False False = PEQ

instance Distributive Bool where
  type Mul Bool = And
  type Add Bool = Xor

instance Lattice Bool where
  type Inf Bool = Or
  type Sup Bool = And
