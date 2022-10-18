module FindDefs where

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f [] = []
mapFilter f (x:xs) = case f x of
    Nothing -> mapFilter f xs
    Just y -> y : mapFilter f xs

lift      :: (a -> b -> Maybe c) -> (Maybe a -> Maybe b -> Maybe c)
lift f Nothing _ = Nothing
lift f _ Nothing = Nothing
lift f (Just x) (Just y) = f x y

compute   :: (Monoid n) => (a -> n) -> [a] -> n
compute f [] = mempty
compute f (x:xs) = f x `mappend` compute f xs

fuse      :: (a -> b -> c) -> (a -> b) -> a -> c
fuse f g x = f x (g x)

