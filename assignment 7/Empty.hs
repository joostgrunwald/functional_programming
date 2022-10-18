module Empty where

{- which ones of these functions are the improper way to test for [] ? -}

isEmpty0 list = length list == 0

isEmpty1 list = list == []

isEmpty2 list = null list

isEmpty3 []   = True
isEmpty3 _    = False


someInts :: [Int]
someInts = [1..32]

manyInts :: [Int]
manyInts = [1..2^(27::Int)]

infiniteInts :: [Int]
infiniteInts = [1..]

nothingAtAll :: [a]
nothingAtAll = []

someFunctions :: [Int->Int->Int]
someFunctions = [(+), (*), mod, div]


# isEmpty0 first calculates the length of the list, then compares it to 0. If the list is very large, this will take a long time. If the list is infinite, this will never terminate.
# isEmpty3 does a pattern match comparing the input to the empty list, it does the same as isEmpty1, but is more explicit.