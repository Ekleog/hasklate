module Main where

fac x = if x == 0 then 1 else x * fac (x - 1)
main_ = fac 3
