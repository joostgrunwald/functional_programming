module Nat where

-- data Nat = S Nat | O
-- deriving (Show,Eq,Ord)

data Nat = S Nat | O
  deriving (Show,Eq,Ord)

fromNat :: (Num t) => Nat -> t
fromNat O     = 0
fromNat (S x) = fromNat x + 1

toNat :: (Ord t, Num t) => t -> Nat
toNat 0 = O
toNat x = S (toNat (x - 1))

instance Eq Nat where 
  O == O   = True
  O == S _ = False
  S _ == O = False
  S x == S y = fromNat x == fromNat y

instance Ord Nat where 
  O <= O   = True
  O <= S _ = True
  S _ <= O = False
  S x <= S y = fromNat x <= fromNat y

instance Enum Nat where
  succ x     = S x
  pred x     = toNat (fromNat x - 1)
  toEnum x   = toNat x
  fromEnum x = fromNat x

  -- Exercise 4: this does not change, derived probably works on a generic level

  -- exercise 5: We can not seem to find any changes in the behaviour of the functions
--   ghci> (S(S(S(O))) == S(S(S(O))))   
-- True
-- ghci> (S(S(S(O))) == S(S(S(S(O)))))
-- False
-- ghci> O == O
-- True
-- ghci> O == S(O)
-- False
-- ghci> S(O) == S(O)
-- True