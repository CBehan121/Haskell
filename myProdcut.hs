myProduct :: [Int] -> Int
myProduct [] = 1
myProduct [a:x] = a * myProduct(x) 