delAll :: Eq a => a -> [a] -> [a] 
delAll a [] = []
delAll a (x:xs)
	| (a == x) = delAll a xs
	| otherwise = x : delAll a xs