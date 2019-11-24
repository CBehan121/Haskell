myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (a : xs) = myProduct(xs) * a