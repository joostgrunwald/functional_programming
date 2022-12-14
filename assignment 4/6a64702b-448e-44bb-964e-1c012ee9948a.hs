module AST where

-- 4.6.1
data Expr = Lit Integer | Add Expr Expr | Mul Expr Expr | Sub Expr Expr | Div Expr Expr | VarX

-- 4.6.2
-- does / give rounding problems?
--eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a
eval (Lit x) _ = Just x
eval (Add x y) z = (+) <$> eval x z <*> eval y z
eval (Mul x y) z = (*) <$> eval x z <*> eval y z
eval (Sub x y) z = (-) <$> eval x z <*> eval y z
eval (Div x y) z = if (eval y z) == Just 0 then Nothing else `div`` <$> eval x z <*> eval y z	
--eval (Div x y) z = eval x z  eval y z
eval (VarX) z = Just z

--or:
--  data Expr = Lit Integer | Expr :+: Expr | Expr :*: Expr | ...
--  infixl 6 :+:
--  infixl 7 :*:


--eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a
