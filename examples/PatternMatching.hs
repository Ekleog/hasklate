module Main where

data MyList = MyNil | MyCons Int MyList

mysum MyNil = 0
mysum (MyCons hd tl) = hd + mysum tl

main_ = mysum (MyCons 3 (MyCons 5 (MyCons 42 MyNil)))
