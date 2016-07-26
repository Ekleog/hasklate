module Main where

data MyBool = MyTrue | MyFalse

foo a MyFalse b = 0
foo c MyTrue d = 1

bar a = 2

main_ = foo 1 MyFalse 2
