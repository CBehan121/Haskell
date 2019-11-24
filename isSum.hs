isSum :: Int -> Int -> Int -> Bool
isSum a b c = (a + b == c) || (b + c == a) || (c + a == b)
