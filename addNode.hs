data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show, Eq, Read) 

addNode :: Ord a => a -> Tree a -> Tree a
addNode x (Null) = Node x Null Null
addNode x (Node a left right)
	| x == a = Node x left right
	| x < a = Node a (addNode x left) right
	| x > a = Node a left (addNode x right)


makeTree :: Ord a => [a] -> Tree a
makeTree [] = Null
makeTree (x:xs) = addNode x (makeTree xs) 
	

















