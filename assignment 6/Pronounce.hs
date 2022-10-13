module Pronounceable where

import Data.List

say :: Integer -> String
say  0 = "zero"
say  1 = "one"
say  2 = "two"
say  3 = "three"
say  4 = "four"
say  5 = "five"
say  6 = "six"
say  7 = "seven"
say  8 = "eight"
say  9 = "nine"
say 10 = "ten"
say 11 = "eleven"
say 12 = "twelve"
say 13 = "thirteen"
say 14 = "fourteen"
say 15 = "fifteen"
say 16 = "sixteen"
say 17 = "seventeen"
say 18 = "eighteen"
say 19 = "nineteen"
say 20 = "twenty"
say 30 = "thirty"
say 40 = "forty"
say 50 = "fifty"
say 60 = "sixty"
say 70 = "seventy"
say 80 = "eighty"
say 90 = "ninety"
say 100 = "hundred"
say 1000 = "thousand"

say x
    -- if smaller then 100 (ex 73) we do 73 - 73 mod 10 = 70 then return REST 3
    | x < 100 = say(x - mod x 10) ++ " " ++ say(x - (10 * div x 10))
    
    -- if smaller then 1000 (ex 343) we do 343 - 3 * 100 = 300 then return REST 43 (we add and keywoard for certain situations)
    | x < 1000 = say(div x 100)
    ++ if (x-(100 * div x 100)) > 0 then " hundred and " ++ say(x - (100 * div x 100)) else " hundred"

-- we add the and keywoard when the difference with the thousand is less then 100
    | x <= 100000 = say(div x 1000)
        ++ if (x-(1000 * div x 1000)) >= 100 then " thousand " ++ say(x - (1000 * div x 1000)) 
        else if (x-(1000 * div x 1000)) > 0 then " thousand and " ++  say(x - (1000 * div x 1000)) 
        else " thousand"

-- big numbers get an extra end when the REST is more then thousand (two hundred and nine thousand)
    | x < 1000000 = say(div x 100000) ++ " hundred " 
        ++ if (x-(100000 * div x 100000)) >= 1000 then "and " ++ say(x - (100000 * div x 100000))
        else say(x - (100000 * div x 100000))

class Pronounceable a where
  pronounce :: a -> String

instance Pronounceable Integer where
    pronounce x = say x
  
instance Pronounceable Int where
    pronounce x = say (toInteger x)

instance Pronounceable Double where
  -- output: 23.0 = twenty three point zero, 37.5 = thirty seven point five
  -- if x is one digit do * 10 if x is two digits do * 100, if x is three digits do * 1000
  pronounce x = say (floor x) ++ " point " ++ say (floor (10^((length (show (floor x)) - 1)) * (x - fromInteger (floor x))))
 
-- an instance of Pronounceable for Double which uses the pronounce instance for
-- Integer to put floating point numbers in words, rounded to the first decimal:
-- pronounce 23.0 ⟹ "twenty three point zero"
-- pronounce 37.5 ⟹ "thirty seven point five"
-- pronounce 3.14 ⟹ "three point one

-- implementation:
-- 1. floor x to get the integer part
-- 2. subtract the integer part from x to get the fractional part
-- 3. multiply the fractional part by 10^((length (show (floor x)) - 1)) to get the first decimal
-- 4. floor the result to get the first decimal
-- 5. pronounce the integer part and the first decimal

-- example: 23.0
-- 1. floor 23.0 ⟹ 23
-- 2. 23.0 - 23 ⟹ 0.0
-- 3. 0.0 * 10^((length (show (floor 23.0)) - 1)) ⟹ 0.0 * 10^((length (show 23) - 1)) ⟹ 0.0 * 10^((2 - 1)) ⟹ 0.0 * 10^1 ⟹ 0.0 * 10 ⟹ 0.0
-- 4. floor 0.0 ⟹ 0
-- 5. pronounce 23 ⟹ "twenty three" ++ " point " ++ pronounce 0 ⟹ "twenty three point zero"

-- example: 37.5
-- 1. floor 37.5 ⟹ 37
-- 2. 37.5 - 37 ⟹ 0.5
-- 3. 0.5 * 10^((length (show (floor 37.5)) - 1)) ⟹ 0.5 * 10^((length (show 37) - 1)) ⟹ 0.5 * 10^((2 - 1)) ⟹ 0.5 * 10^1 ⟹ 0.5 * 10 ⟹ 5.0
-- 4. floor 5.0 ⟹ 5
-- 5. pronounce 37 ⟹ "thirty seven" ++ " point " ++ pronounce 5 ⟹ "thirty seven point five"



instance Pronounceable Char where
  pronounce c = unwords ["the","character", "'"++[c]++"'"]

instance Pronounceable a => Pronounceable [a] where
  pronounce xs = "a list containing " ++ (intercalate ", " $ map pronounce xs)

-- instance for tuples
instance (Pronounceable a, Pronounceable b) => Pronounceable (a,b) where
  pronounce (x,y) = "a tuple containing " ++ pronounce x ++ " and " ++ pronounce y