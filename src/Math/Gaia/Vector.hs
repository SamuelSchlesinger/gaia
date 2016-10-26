{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Tower.Vector
    ( Vector(..)
    )
    where

import qualified Protolude as P
import Protolude (Applicative(..), ($), (<$>), (<*>))
import Math.Gaia

newtype Vector f a = Vector { unvec :: (Applicative (Vector f)) => f a}

newtype AddVector f a = AddVector { unvecAdd :: (Applicative (Vector f)) => Vector f a }
newtype MulVector f a = MulVector { unvecMul :: (Applicative (Vector f)) => Vector f a }

instance (Distributive a) =>
    Magma (AddVector f a) where
    AddVector a `mul` AddVector b  = AddVector ((+) <$> a <*> b)

instance (Distributive a) =>
    Unital (AddVector f a) where
    unit = AddVector zero

instance (Distributive a) =>
    Magma (MulVector f a) where
    MulVector a `mul` MulVector b  = MulVector ((*) <$> a <*> b)

instance (Distributive a) =>
    Unital (MulVector f a) where
    unit = MulVector zero

instance (
    Distributive a,
    Applicative (Vector f)
    ) =>
    Distributive (Vector f a) where
    type Add (Vector f a) = AddVector f a
    type Mul (Vector f a) = MulVector f a
