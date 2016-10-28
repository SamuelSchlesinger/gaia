module Main where

import Prelude hiding (Num(..), (++))
import Data.Array.IArray
import Data.Array ()
import Math.Gaia
import Math.Gaia.Int
import Math.Gaia.Float
import Math.Gaia.Bool
import Math.Gaia.Integer

identity :: (Distributive x, IArray array x)
         => Int -> array (Int, Int) x

identity n = array ((1, 1), (n, n)) [ if i == j then ((i, j), one) else ((i, j), zero)
                                    | i <- [1..n], j <- [1..n] ]

matmul :: (Distributive x, IArray array x)
    => array (Int, Int) x -> array (Int, Int) x -> array (Int, Int) x

matmul a b = let
  ((1, 1), (aw, ah)) = bounds a
  ((1, 1), (bw, bh)) = bounds b
  in if aw /= bh 
  then error "cmaaaahn"
  else accumArray (+) zero ((1, 1), (bw, ah)) 
       [ (ind, a ! ind * b ! ind) 
       | x <- [1..bw], y <- [1..ah], k <- [1..aw]
       , let ind = (x, y) ]

main = do
  let e = (identity 10 :: Array (Int, Int) Integer)
  print "heh"
