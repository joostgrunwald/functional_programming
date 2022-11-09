module Expression where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

skewed :: Integer -> Tree ()
skewed 0 = Leaf
skewed n = Node () (skewed (n-1)) Leaf

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node x lt rt) = inorder lt ++ [x] ++ inorder rt

{-
Derive
  inorderCat t xs = inorder t ++ xs

  inorderCat (Node x lt rt) xs

  inorderCat lt (x:inorderCat rt xs)

  inorder lt ++ x:inorderCat rt xs

  inorder lt ++ x:inorder rt ++ xs

  inorder lt ++ [x] ++ inorder rt ++ xs

  inorder (Node x lt rt) ++ xs

  inorderCat (Node x lt rt) xs = inorder (Node x lt rt) ++ xs

  inorderCat t xs = inorder t ++ xs

  Q.E.D.
-}

inorderCat :: Tree a -> [a] -> [a]
inorderCat Leaf xs = xs
inorderCat (Node x l r) xs = inorderCat l [x] ++ inorderCat r xs

inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

-- TODO: make me more efficient, too
elems :: Tree a -> [a]
elems Leaf xs = []
elems (Node x lt rt) = x : elems lt ++ elems rt

elemsCat :: Tree a -> [a] -> [a]
elemsCat Leaf xs = xs
elemsCat (Node x l r) xs = x : elemsCat l (elemsCat r xs)

{-
Derive elemsCat t xs = elems t ++ xs
  
    elemsCat (Node x lt rt) xs
  
    elemsCat lt (x:elemsCat rt xs)
  
    elems lt ++ x:elemsCat rt xs
  
    elems lt ++ x:elems rt ++ xs
  
    elems lt ++ [x] ++ elems rt ++ xs
  
    elems (Node x lt rt) ++ xs
  
    elemsCat (Node x lt rt) xs = elems (Node x lt rt) ++ xs
  
    elemsCat t xs = elems t ++ xs
  
    Q.E.D.
-}



