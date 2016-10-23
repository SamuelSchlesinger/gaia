{-# OPTIONS_GHC -Wall #-}

module Main where

import Protolude hiding ((+),(-),(*),(/),zero,one,negate,div,mod,rem,quot, Integral(..), Semiring(..), (==), Integer(..), Bool(..))
import Test.Tasty (TestName, TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck
import Gaia.Abstract
import Gaia.Orphans.Bool
import Gaia.Orphans.Integer

data LawArity a =
    Unary (a -> Logic a) |
    Binary (a -> a -> Logic a) |
    Ternary (a -> a -> a -> Logic a) |
    Ornary (a -> a -> a -> a -> Logic a)

type Law a = (TestName, (LawArity a))
 
testLawOf  ::
    ( Arbitrary a
    , Show a
    , Testable (Logic a)
    ) =>
    [a] -> Law a -> TestTree
testLawOf _ (name, Unary f) = testProperty name f
testLawOf _ (name, Binary f) = testProperty name f
testLawOf _ (name, Ternary f) = testProperty name f
testLawOf _ (name, Ornary f) = testProperty name f

tests :: TestTree
tests = testGroup "everything" $
    [ -- testGroup "Integer" $ testLawOf ([]::[Integer]) <$> laws
    ]

main :: IO ()
main = defaultMain tests
 
laws ::
    ( Equivalence a
    , Magma (Addition a)
    , Magma (Multiplication a)
    , Coercible (Addition a) a
    , Coercible a (Addition a)
    , Coercible (Multiplication a) a
    , Coercible a (Multiplication a)
    -- , Distributive a
    ) =>
    [Law a]
laws =
    [ ("semigroup: a + b = b + a", Ternary (\a b c -> ((a + b) + c) == (a + (b + c))))
    --   ("monoid leftid: zero + a = a", Unary (\a -> neutral + a == a))
    -- , ("monoid rightid: a + zero = a", Unary (\a -> a + neutral == a))
    -- , ("group rightminus1: (a + b) - b = a", Binary (\a b -> (a + b) - b == a))
    -- , ("group rightminus2: a + (b - b) = a", Binary (\a b -> a + (b - b) == a))
    -- , ("group negateminus: a + negate b == a - b", Binary (\a b -> a + negate b == a - b))
    -- , ("group leftinverse: negate a + a == zero", Unary (\a -> negate a + a == zero))
    -- , ("group rightinverse: a + negate a == zero", Unary (\a -> a + negate a == zero))
    -- , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
    -- , ("times associativity: (a * b) * c == a * (b * c)", Ternary (\a b c -> (a * b) * c == a * (b * c)))
    -- , ("times commutivity: a * b == b * a", Binary (\a b -> a * b == b * a))
    -- , ("annihilation: a * zero == zero", Unary (\a -> a * neutral == neutral))
    -- , ("left distributivity: a * (b + c) == a * b + a * c", Ternary (\a b c -> a * (b + c) == a * b + a * c))
    -- , ("right distributivity: (a + b) * c == a * c + b * c", Ternary (\a b c -> (a + b) * c == a * c + b * c))
    -- , ("times id: a * one == a && one * a == a", Unary (\a -> a * 1 == a && 1 * a == a))
    ]
