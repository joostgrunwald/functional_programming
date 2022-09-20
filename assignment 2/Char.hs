module Char where

import Data.Char

-- Exercise 2.5.1
(~~) :: String -> String -> Bool
(~~) a b = (map toLower a) == (map toLower b)

-- Exercise 2.5.2
-- little helper function that reverses a char
reverseC :: Char -> Char
reverseC c = if isUpper c then toLower c else toUpper c

-- the reverse on the string is a map on the reverseC function
reverseCase :: String -> String
reverseCase a = (map reverseC a)

-- Exercise 2.5.3

-- check if it overflows after the addition, if it does simply substract the max ascii number
shifter :: Int -> Char -> Char
shifter p a 
    | ((ord a) + p) > 90 = toUpper(chr((ord a) + p - 26))
    | ((ord a) + p) <= 90 = toUpper(chr((ord a) + p))

shift :: Int -> Char -> Char
shift p a = if isAscii a then (if isUpper a then shifter p a else a) else a

-- first check if the input is an actual ascii number else return input
-- if it is an ascii number check if it is uppercase else return inpshut
-- if it is upercase ascii convert it to int and add p to it
-- then convert it back to char and make sure it is upper.

--caesar :: Int -> String -> String
caesar :: Int -> String -> String
caesar p a = (map (shift p) a)

decodemsg :: String -> Int -> String
decodemsg a p = (caesar (p) a)

-- the magical number of p was 5
-- decodemsg msg 5 gives "FIRST I MUST SPRINKLE YOU WITH FAIRY DUST"
msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"
