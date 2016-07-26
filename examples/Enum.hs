module Main where

data MyBool = MyTrue | MyFalse

foo MyFalse = 0
foo MyTrue = 1

main_ = foo MyTrue
