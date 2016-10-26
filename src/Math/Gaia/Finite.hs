{-# LANGUAGE DataKinds, PolyKinds, GADTs, FlexibleContexts, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances, ConstraintKinds, StandaloneDeriving #-}

module Math.Gaia.Finite 
  (
    Mod(..)
  , AddMod(..)
  , MulMod(..)
  , Prime(..)
  , RootOf(..)
  ) where

import Prelude hiding ((+), (-), (*), (/), exp, div, mod, Monoid(..), negate)
import GHC.TypeLits
import Data.Proxy
import Data.Coerce
import Data.Kind (Type)
import Data.Type.Equality
import Math.Gaia

{-# SPECIALIZE (+) :: (IntegralDomain z, Homomorphic Integer z, KnownNat n) => z `Mod` n -> z `Mod` n -> z `Mod` n #-}
{-# SPECIALIZE (*) :: (IntegralDomain z, Homomorphic Integer z, KnownNat n) => z `Mod` n -> z `Mod` n -> z `Mod` n #-}

data Mod :: Type -> Nat -> Type where
  Mod :: !z -> Mod z n

deriving instance Show z => Show (Mod z n)

instance (
    Eq z
  , IntegralDomain z
  , Homomorphic Integer z
  , KnownNat n
  ) => Eq (Mod z n) where
  {-# INLINE (==) #-}
  Mod x == Mod y = x `mod` hom (natVal (Proxy :: Proxy n)) == x `mod` hom (natVal (Proxy :: Proxy n))

instance (
    Ord z
  , IntegralDomain z
  , Homomorphic Integer z
  , KnownNat n
  ) => Ord (Mod z n) where
  {-# INLINE compare #-}
  compare (Mod x) (Mod y) = compare (x `mod` (hom (natVal (Proxy :: Proxy n)) :: z)) 
                                    (y `mod` (hom (natVal (Proxy :: Proxy n)) :: z))

newtype AddMod z n = AddMod (Mod z n)

instance (IntegralDomain z, KnownNat n, Homomorphic Integer z) => Magma (AddMod z n) where
  mul (AddMod (Mod a)) (AddMod (Mod b)) = AddMod $ Mod ((a + b) `mod` (hom (natVal (Proxy :: Proxy n)) :: z))


instance (
    IntegralDomain z
  , KnownNat n
  , Homomorphic Integer z
  ) => Associative (MulMod z n)

instance (
    IntegralDomain z
  , KnownNat n
  , Homomorphic Integer z
  ) => Associative (AddMod z n)

instance (
    IntegralDomain z
  , KnownNat n
  , Homomorphic Integer z
  ) => Unital (AddMod z n) where
  {-# INLINE unit #-}
  unit = AddMod $ (Mod (hom (1 :: Integer)) :: Mod z n)

instance (
    IntegralDomain z
  , KnownNat n
  , Homomorphic Integer z
  ) => Unital (MulMod z n) where
  {-# INLINE unit #-}
  unit = MulMod $ (Mod (hom (1 :: Integer)) :: Mod z n)

newtype MulMod z n = MulMod (Mod z n)

instance (
    IntegralDomain z
  , KnownNat n
  , Homomorphic Integer z
  ) => Magma (MulMod z n) where
  mul (MulMod (Mod a)) (MulMod (Mod b)) = MulMod $ Mod ((a * b) `mod` (hom (natVal (Proxy :: Proxy n)) :: z))


instance (
    IntegralDomain z
  , KnownNat n
  , Homomorphic Integer z
  ) => Distributive (Mod z n) where
  type Add (Mod z n) = AddMod z n
  type Mul (Mod z n) = MulMod z n

instance (
    Ord z
  , IntegralDomain z
  , KnownNat n
  , Homomorphic Integer z
  ) => POrd (Mod z n) where
  pcompare a b = ord2pord $ compare a b

type family If (c :: Bool) (a :: k) (b :: k) :: k where
  If True a b = a
  If False a b = b

-- | This doesn't work for small numbers or something
type family IsPrime (n :: Nat) (m :: Nat) (k :: Nat) :: Bool where
  IsPrime n 1 k = True
  IsPrime n m 1 = IsPrime n (m - 1) (m - 2)
  IsPrime n m k = If (k * m == n) False (IsPrime n m (k - 1))

type Prime n = IsPrime n (n - 1) (n - 2) ~ True

type family IsRootOf (n :: Nat) (m :: Nat) (p :: Nat) :: Bool where
  IsRootOf n m p = If (n == m) True (If (n <=? m) False (IsRootOf n (m * p) p))

type RootOf n p = IsRootOf n p p



