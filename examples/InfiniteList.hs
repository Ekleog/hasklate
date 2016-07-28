module Main where

data MyList = MyNil | MyCons Int MyList

enumerate x = MyCons x (enumerate (x + 1))

take x MyNil = MyNil
take x (MyCons y l) = if x == 0 then MyNil else MyCons y (take (x - 1) l)

sum MyNil = 0
sum (MyCons hd tl) = hd + sum tl

main_ = sum (take 5 (enumerate 0))
