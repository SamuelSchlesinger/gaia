{-# OPTIONS_GHC -Wall #-}

module Main where

import Protolude hiding ((+),(-),(*),(/),zero,one,negate,div,mod,rem,quot, Integral(..), Semiring(..), Bool(..),(<>), Semigroup, empty)

import Test.Tasty (TestName, TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck

import Math.Gaia
import Math.Gaia.Integer()
import Math.Gaia.Bool

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
    , Num a
    , Distributive a
    ) =>
    [Law a]
laws =
    [ ("associative: a + (b + c) == (a + b) + c", Ternary (\a b c -> a + (b + c) == (a + b) + c))
    , ("left id: 0 + a = a", Unary (\a -> 0 + a == a))
    , ("right id: a + 0 = a", Unary (\a -> a + 0 == a))
    , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
    , ("associative: (a * b) * c == a * (b * c)", Ternary (\a b c -> (a * b) * c == a * (b * c)))
    , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
    , ("left annihilative: a * 0 == 0", Unary (\a -> a * 0 == 0))
    , ("right annihilative: 0 * a == 0", Unary (\a -> 0 * a == 0))
    , ("left distributive: a * (b + c) == a * b + a * c", Ternary (\a b c -> a * (b + c) == a * b + a * c))
    , ("right distributive: (a + b) * c == a * c + b * c", Ternary (\a b c -> (a + b) * c == a * c + b * c))
    , ("left id: 1 * a == a", Unary (\a -> 1 * a == a))
    , ("right id: a * 1 == a", Unary (\a -> a * 1 == a))
    -- , ("group rightminus1: (a + b) - b = a", Binary (\a b -> (a + b) - b == a))
    -- , ("group rightminus2: a + (b - b) = a", Binary (\a b -> a + (b - b) == a))
    -- , ("group negateminus: a + negate b == a - b", Binary (\a b -> a + negate b == a - b))
    -- , ("group leftinverse: negate a + a == zero", Unary (\a -> negate a + a == zero))
    -- , ("group rightinverse: a + negate a == zero", Unary (\a -> a + negate a == zero))
    ]

laws' :: [Law Integer]
laws' =
    [ ("associative: a + (b + c) == (a + b) + c", Ternary (\a b c -> a + (b + c) == (a + b) + c))
    , ("left id: 0 + a = a", Unary (\a -> 0 + a == a))
    , ("right id: a + 0 = a", Unary (\a -> a + 0 == a))
    , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
    , ("associative: (a * b) * c == a * (b * c)", Ternary (\a b c -> (a * b) * c == a * (b * c)))
    , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
    , ("left annihilative: a * 0 == 0", Unary (\a -> a * 0 == 0))
    , ("right annihilative: 0 * a == 0", Unary (\a -> 0 * a == 0))
    , ("left distributive: a * (b + c) == a * b + a * c", Ternary (\a b c -> a * (b + c) == a * b + a * c))
    , ("right distributive: (a + b) * c == a * c + b * c", Ternary (\a b c -> (a + b) * c == a * c + b * c))
    , ("left id: 1 * a == a", Unary (\a -> 1 * a == a))
    , ("right id: a * 1 == a", Unary (\a -> a * 1 == a))
    -- , ("group rightminus1: (a + b) - b = a", Binary (\a b -> (a + b) - b == a))
    -- , ("group rightminus2: a + (b - b) = a", Binary (\a b -> a + (b - b) == a))
    -- , ("group negateminus: a + negate b == a - b", Binary (\a b -> a + negate b == a - b))
    -- , ("group leftinverse: negate a + a == zero", Unary (\a -> negate a + a == zero))
    -- , ("group rightinverse: a + negate a == zero", Unary (\a -> a + negate a == zero))
    ]
