nubSorted :: Eq a => [a] -> [a]
nubSorted [] = []
nubSorted [a] = [a]
nubSorted (x:y:xs)
		|(x == y) = y : nubSorted (xs)
		|otherwise = x : nubSorted (y:xs)