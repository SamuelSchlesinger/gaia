{-# LANGUAGE 
    NoImplicitPrelude 
  , TypeFamilies 
  , LambdaCase 
  , FlexibleInstances 
  , MultiParamTypeClasses #-}

module Gaia.Orphans.Bool 
  (
    Bool(..)
  )where

import Gaia.Abstract
import Data.Bool (Bool(..))

type instance Logic Bool = Bool
type instance Multiplication Bool = And
type instance Addition Bool = Xor
type instance Join Bool = Or
type instance Meet Bool = And

instance Propositional Bool where
  not True = False
  not False = True
  and True True = True
  and _ _ = False
  or False False = False
  or _ _ = True
  true = True
  false = False

instance Conditional Bool x where
  ifThenElse True a b = a
  ifThenElse False a b = b

instance Equivalence Bool where
  True == True = True
  False == False = True
  _ == _ = False

newtype Xor = Xor Bool
newtype Or  = Or  Bool
newtype And = And Bool

instance Magma Xor where
  Xor a `mul` Xor b = Xor (a `xor` b)
instance Magma Or where
  Or a `mul` Or b = Or (a `or` b)
instance Magma And where
  And a `mul` And b = And (a `and` b)

instance Neutral Xor where
  neutral = Xor False
instance Neutral Or where
  neutral = Or False
instance Neutral And where
  neutral = And True

instance Idempotent And
instance Idempotent Or

instance Semigroup Xor
instance Semigroup And
instance Semigroup Or

instance Commutative Or
instance Commutative And
instance Commutative Xor

instance Invertible Xor where
  inv x = x
