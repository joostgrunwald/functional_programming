module Say where
    
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
