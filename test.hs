module Main where

fac x = if x == 0 then 1 else x * fac (x - 1)
add = (+)
foo x = add x 2
-- main_ = foo $ fac 5
-- main_ = foo (fac 5)
main_ = fac 5
