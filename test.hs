module Main where

fac x = if x == 0 then 1 else x * fac (x - 1)
doit a b = fac a + fac b
thething f = f 3
main_ = thething (doit 4)
