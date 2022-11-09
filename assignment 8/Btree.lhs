> module Btree where
>
> data Btree a = Tip a | Bin (Btree a) (Btree a)
>
> mapBtree :: (a -> b) -> Btree a -> Btree b
> mapBtree f (Tip a)     = Tip (f a)
> mapBtree f (Bin t1 t2) = Bin (mapBtree f t1) (mapBtree f t2)

> tips :: (Btree a) -> [a]
> tips (Tip x) = [x]
> tips (Bin as bs) = tips as ++ tips bs

To prove: map f (tips t) = tips (mapBtree f t) for all f,t

By induction on t.

-- Case 1: t = Tip x
-- map f (tips (Tip x))
-- definition of tips
-- = map f [x]
-- definition of map
-- = [f x]
-- definition of tips
-- tips (Tip (f x))
-- definition of mapBtree
-- = tips (mapBtree f (Tip x))

-- Case 2: t = Bin as bs
-- IH: map f (tips as) = tips (mapBtree f as)
-- map f (tips (Bin as bs))
-- definition of tips
-- map f (tips as ++ tips bs)
-- definition of ++
-- map f (tips as) ++ map f (tips bs)
-- definition of map
-- map f (tips as) ++ map f (tips bs)
-- induction hypothesis
-- tips (mapBtree f as) ++ tips (mapBtree f bs)
-- definition of tips
-- tips (Bin (mapBtree f as) (mapBtree f bs))
-- definition of mapBtree
-- tips (mapBtree f (Bin as bs)) (where t = Bin as bs)
-- tips (mapBtree f t)