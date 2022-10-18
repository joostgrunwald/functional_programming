module PolyTypes where

-- we will derive the most general type of these polymorphic functions
-- the most general type of justs xs is 

justs:: [Maybe Char] -> [Char]
justs xs           = [ x | Just x <- xs, x /= ' ' ]

-- the output is a list [], we take a list xs and apply a function to this

-- max and min can be used on integers, floats, doubles, type num?
orderPairs :: (Ord a) => [(a,a)] -> [(a,a)]
orderPairs xs      = map (\(x,y)->(min x y, max x y)) xs

unmaybe :: Maybe (Maybe a) -> Maybe a
unmaybe (Just x)   = x
unmaybe Nothing    = Nothing

accumulate :: (t -> (a, t)) -> t -> [a] 
accumulate f st    = let (x,st') = f st in x : accumulate f st'
