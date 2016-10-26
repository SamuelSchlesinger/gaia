{-# OPTIONS_GHC -Wall #-}
module Gaia.Prelude (module X) where

import Protolude as X hiding
    ( Semiring(..)
    , (+)
    , (-)
    , (*)
    , (/)
    -- , cancel
    , zero
    , one
    , negate
    , div
    , mod
    , abs
    , infinity
    , exp
    )

import Math.Gaia as X hiding
    ( (<>)
    , (++)
    , empty
    , Semigroup
    , Monoid
    )

import Math.Gaia.Integer as X
import Math.Gaia.Float as X
import Math.Gaia.Bool as X
import Math.Gaia.Finite as X

