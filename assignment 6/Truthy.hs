module Truthy where

class Truthy a where
    truthy :: a -> Bool

instance Truthy Bool where
    truthy True = True
    truthy False = False

instance Truthy Integer where
    truthy 0 = False
    truthy x = (x > 0)

data Nope = Nope
    deriving (Show, Eq)

instance Truthy Nope where
    truthy Nope = False

instance (Truthy a, Truthy b) => Truthy (a, b) where
    truthy (a, b) = truthy a || truthy b

(&&&) :: (Truthy a, Truthy b) => a -> b -> Bool
(&&&) a b = truthy a && truthy b

(|||) :: (Truthy a, Truthy b) => a -> b -> Bool
(|||) a b = truthy a || truthy b

ifThenElse :: (Truthy a) => a -> b -> b -> b
ifThenElse a b c = if truthy a then b else c

