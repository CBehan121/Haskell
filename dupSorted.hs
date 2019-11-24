dupSorted :: Eq a => [a] -> Bool
dupSorted [] = False
dupSorted [a] = False
dupSorted (x:y:xs)
		|(x == y) = True
		|otherwise = dupSorted (y:xs)