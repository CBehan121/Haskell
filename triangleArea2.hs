triangleArea :: Float -> Float -> Float -> Float
triangleArea n m p
    | not(n+m>=p && m+p>=n && p+n>=m) = error "Not a triangle!"
    | otherwise = (sqrt(s*(s - n)*(s - m)*(s - p))) 

    where 
           s = (n + m + p)/2
