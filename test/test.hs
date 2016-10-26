{-# OPTIONS_GHC -Wall #-}

module Main where

import Protolude hiding ((+),(-),(*),(/),zero,one,negate)

import Test.Tasty (TestName, TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck

import Math.Gaia
import Math.Gaia.Integer()
import Math.Gaia.Bool()

data LawArity a =
    Unary (a -> Bool) |
    Binary (a -> a -> Bool) |
    Ternary (a -> a -> a -> Bool) |
    Ornary (a -> a -> a -> a -> Bool)

type Law a = (TestName, (LawArity a))
 
testLawOf  ::
    ( Arbitrary a
    , Show a
    ) =>
    [a] -> Law a -> TestTree
testLawOf _ (name, Unary f) = testProperty name f
testLawOf _ (name, Binary f) = testProperty name f
testLawOf _ (name, Ternary f) = testProperty name f
testLawOf _ (name, Ornary f) = testProperty name f

tests :: TestTree
tests = testGroup "everything" $
    [ testGroup "Integer" $ testLawOf ([]::[Integer]) <$> laws
    ]

main :: IO ()
main = defaultMain tests

laws ::
    ( Eq a
    , Distributive a
    , Invertible (Add a)
    , Commutative (Add a)
    , Lattice a
    , Homomorphic (Inf a) (Sup a)
    ) =>
    [Law a]
laws =
    [ ("associative: a + (b + c) == (a + b) + c", Ternary (\a b c -> a + (b + c) == (a + b) + c))
    , ("left zero: zero + a = a", Unary (\a -> zero + a == a))
    , ("right zero: a + zero = a", Unary (\a -> a + zero == a))
    , ("left one: one * a == a", Unary (\a -> one * a == a))
    , ("right one: a * one == a", Unary (\a -> a * one == a))
    , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
    , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
    , ("associative: (a * b) * c == a * (b * c)", Ternary (\a b c -> (a * b) * c == a * (b * c)))
    , ("left annihilative: a * zero == zero", Unary (\a -> a * zero == zero))
    , ("right annihilative: zero * a == zero", Unary (\a -> zero * a == zero))
    , ("left distributive: a * (b + c) == a * b + a * c", Ternary (\a b c -> a * (b + c) == a * b + a * c))
    , ("right distributive: (a + b) * c == a * c + b * c", Ternary (\a b c -> (a + b) * c == a * c + b * c))
    , ("right minus1: (a + b) - b = a", Binary (\a b -> (a + b) - b == a))
    , ("right minus2: a + (b - b) = a", Binary (\a b -> a + (b - b) == a))
    , ("negate minus: a + negate b == a - b", Binary (\a b -> a + negate b == a - b))
    , ("left inverse: negate a + a == zero", Unary (\a -> negate a + a == zero))
    , ("right inverse: a + negate a == zero", Unary (\a -> a + negate a == zero))
    ]
