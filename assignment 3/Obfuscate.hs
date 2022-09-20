module Obfuscate where

import Data.Char
import System.Random (getStdGen, randomRIO)
import Data.List (permutations)


--cambridge :: String -> String

meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a wohle."

mainfunc = map (cambridge) (toWords(meme))

--cambridge :: String -> String
-- get first letter and last letter
-- get the rest of the letters
-- reverse the rest of the letters
-- put the first letter and last letter back
-- return the string
cambridge [] = []
cambridge (x:xs) = x : init (reverse xs) ++ [last xs]

toWords :: String -> [String]
toWords [] = []
toWords (x:xs) = if x == ' ' then toWords xs else (x:takeWhile (/= ' ') xs) : toWords (dropWhile (/= ' ') xs)
