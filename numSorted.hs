numSorted :: Ord a => a -> [a] -> Int
numSorted a [] = 0 
numSorted a (x:xs) 
		| x == a = 1 + (numSorted a xs)
		| x > a = 0
		| otherwise = numSorted a xs