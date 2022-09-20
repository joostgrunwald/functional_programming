module PolyType where

-- ad hoc polymorphism (because x <= y compares value for integers and length for strings/lists/arrays)
-- no type classes, works for Num, integers etc
f8 :: a -> a -> a
f8 x y  = if x <= y then x else y

-- not polymorphic
f9 :: Bool -> Bool -> Bool
f9 x y  = not x || y

-- parametric polymorphism
-- the type class is Num (it takes integer, int, float, doable)
f10 x y
  | x == 0    = y
  | otherwise = x + y

-- parametric polymorphism
-- no type classes, works for everything and just returns the first agument
f11 x y = get 0
  where get n = if n == 0 then x else y

-- Exercise 2.4.1
-- Functions f8 and f11 can be used on arguments of type string.

-- Exercise 2.4.2
-- see annotations at functions
