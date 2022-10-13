module DigitalSorting where

import Data.List
import Data.Char
import Data.Map
import Data.Maybe
import Data.Either
import qualified Data.Bifunctor

class Rankable key where
  rank :: [(key,a)] -> [[a]]

digitalSortOn :: (Rankable key) => (v -> key) -> [v] -> [v]
digitalSortOn f = concat . rank . Data.List.map (\x->(f x, x))

digitalSort :: (Rankable key) => [key] -> [key]
digitalSort = digitalSortOn id

-- genericRank ranks values based on the provided key using a comparison-based approach. To be
-- more precise, the function genericRank should produce a list of lists such that:
-- • values end up in the same list if and only if they had identical keys; and
-- • the lists themselves are presented in order, based on the key their values had
genericRank :: (Ord key) => [(key,a)] -> [[a]]
genericRank key = [b|(a, b) <- toList (fromListWith (++) [(a, [b]) | (a, b) <- key])]

-- instance for Int
instance Rankable Int where
  rank = genericRank

-- instance for Integer
instance Rankable Integer where
  rank = genericRank

-- instance for Char
instance Rankable Char where
  rank = genericRank
-- do this by going over the
-- list and collecting all values associated with the key False, and all the values associated
-- with the key True. This is called a distribution sort or bucket sort.

-- instance for Bool (custom)
instance Rankable Bool where
    rank k = [[t | (True, t) <- k], [f | (False, f) <- k]]

instance (Rankable kеy1, Rankable kеy2) => Rankable (kеy1, kеy2) where
  rank = concatenate rank . rank . Data.List.map assoc where
    assoc :: ((k1, k2), a) -> (k1, (k2, a))
    assoc ((k1,k2),a) = (k1,(k2,a))

concatenate :: (Foldable t1, Foldable t2) => (a1 -> t2 a2) -> t1 a1 -> [a2]
concatenate f = Data.List.foldr (flip (Data.List.foldr (:)) . f) []

-- ! Exercise 6.6.5
instance (Rankable key) => Rankable (Maybe key) where
  rank k = ([n | (Nothing, n) <- k]) : rank [(Just key2, g) | (key2, g) <- k, isJust key2]

-- ! Exercise 6.6.6
instance (Rankable kеy) => Rankable [kеy] where
  rank = rank . Data.List.map (Data.Bifunctor.first uncons)

----------------------------------------------------------------------------------------------------
-- some test inputs (it would be reasonably for "rank" and "genericRank" to produce the same output)

testPhrase :: [Char]
testPhrase = "Hello, world!"

boolTest :: [(Bool,Char)]
boolTest = [ (isLetter c, c) | c <- testPhrase ]

maybeTest :: [(Maybe Char,Char)]
maybeTest = [ (if isLetter c then Just c else Nothing, c) | c <- testPhrase ]

tupleTest :: [((Bool,Char),Char)]
tupleTest = [ ((isLetter c, c), c) | c <- testPhrase ]

listTest :: [(String,Char)]
listTest = [ (w, c) | w <- groupBy (\x y->isLetter x==isLetter y) testPhrase, c <- w ]
