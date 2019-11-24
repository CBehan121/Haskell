triangleArea :: Float -> Float -> Float -> Float
triangleArea n m p = sqrt(s*(s - n)*(s - m)*(s - p))
    where s = (n + m + p)/2
