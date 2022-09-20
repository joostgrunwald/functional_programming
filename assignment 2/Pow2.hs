module Pow2 where

--pow2 :: Integer -> Integer
pow2 :: (Ord n, Num n, Num a) => n -> a
pow2 0 = 1
pow2 n = 2 * pow2 (n - 1)

-- pow2 1 = 2
-- pow2 2 = 4
-- pow2 3 = 8
-- pow2 4 = 16
-- pow2 5 = 32
-- pow2 n = 2 * pow2 (n - 1)


--after finishing the function, change the type to: 
--pow2 :: (Ord n, Num n, Num a) => n -> a

-- exercise 2.3.3, the maximum size is 62, after that it overflows
-- ghci> pow2 64 :: Int
-- 0
-- ghci> pow2 63 :: Int 
-- -9223372036854775808
-- ghci> pow2 62 :: Int
-- 4611686018427387904
-- ghci> pow2 61 :: Int
-- 2305843009213693952
-- ghci> pow2 60 :: Int
-- 1152921504606846976