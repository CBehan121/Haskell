evalPoly :: Int -> [Int] -> Int
evalPoly y [] = 0
evalPoly y (x:xs) = x + y * (evalPoly y xs)