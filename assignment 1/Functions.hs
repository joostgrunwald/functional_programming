
smallestInteger :: Integer -> Integer -> Integer
smallestInteger m n
    | m < n = m
    | otherwise = n

stringNtimes :: String -> Integer -> String
stringNtimes s n
    | n <= 0 = ""
    | otherwise = s ++ stringNtimes s (n-1)

recursiveModulus :: Integer -> Integer -> Integer
recursiveModulus x 0 = x
recursiveModulus x y = recursiveModulus y (x `mod` y)

