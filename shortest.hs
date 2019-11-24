shortest :: [[a]] -> [a]
shortest [] = []
shortest [a] = a
shortest (x:y:list)
    | length x > length y  =  shortest $ y : list 
    | otherwise = shortest $ x : list