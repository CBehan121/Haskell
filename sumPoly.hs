sumPoly :: [Int] -> [Int] -> [Int]
sumPoly [] []= []
sumPoly [] bs = bs
sumPoly ys [] = ys
sumPoly (x:xs) (z:zs) = x+z : sumPoly xs zs