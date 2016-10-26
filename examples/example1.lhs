> {-# LANGUAGE OverloadedStrings #-}
> import Protolude hiding ((+), (-), (*), (/))
> import Math.Gaia hiding ((<>))
> import Math.Gaia.Integer

> x :: Integer
> x = 1+1
> p :: Text -> Integer -> IO ()
> p label code = putStrLn $ label <> show code
> 
> main = do
>   p "1 + 1 = " $ 1 + 1
>   p "1 + 2 + 3 = " $ 1 + 2 + 3
>   -- p "1 + 1 - 1 = " $ 1 + 1 - 1
>   p "1 + 1 * 1 = " $ 1 + 1 * 1
> 
