leap :: Int -> Bool

leap (x)
	| x `mod` 100 + x `mod` 400 == 0 = True
	| x `mod` 100 == 0 = False 
	| x `mod` 4 == 0 = True
	| otherwise =  False



mlengths :: Int -> [Int]
mlengths (y)
	| leap y == True = [31,28,31,30,31,30,31,31,30,31,30,31]
	| otherwise = [31,29,31,30,31,30,31,31,30,31,30,31]