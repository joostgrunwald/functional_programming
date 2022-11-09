Local definitions:

> import Prelude hiding (++)
> 
> (++) :: [a] -> [a] -> [a]
> []     ++ ys = ys
> (x:xs) ++ ys = x : xs++ys

-----------------------------------------------------
To prove: xs ++ [] = xs
By induction on xs.

Case 1: xs = []

    [] ++ []
    -------- definition of (++)
  = []

Case 2: xs = (a:as)
IH: as ++ [] = as

    (a:as) ++ []
    ------------ definition of ++
  = a : as++[]
        ------ IH
  = a : as


-----------------------------------------------------
To prove: xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
By induction on xs

Case 1: xs = []

    [] ++ (ys ++ zs)
    ---------------- definition of ++
  = ys ++ zs
    --               definition of ++
  = ([] ++ ys) ++ zs

Case 2: xs = (a:as)
IH: as ++ (ys ++ zs) = (as ++ ys) ++ zs, for all ys and zs

    (a:as) ++ (ys ++ zs)
    --------------------  definition of ++
  = a : (as ++ (ys ++ zs))
  
  = ...?

  = (a:(as ++ ys)) ++ zs
    --------------         definition of ++
  = ((a:as) ++ ys) ++ zs


-- exercise 8.1.1
-- left-identity follows directly from the definition of (++)
-- right-identity and associativity require induction.

-- exercise 8.1.2
-- To Prove xs ++ [] = xs

-- Case 1: xs = []
-- [] ++ []
-- -------- definition of (++)
-- []
-- Q.E.D.

-- Case 2: xs = (a:as)
-- (a:as) ++ []
-- ------------ definition of (++)
-- a : (as ++ [])
-- a : as
-- Q.E.D.

-- To Prove xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

-- Case 1: xs = []
-- [] ++ (ys ++ zs)
-- ---------------- definition of (++)
-- ys ++ zs
-- [] ++ ys ++ zs
-- --------------- definition of (++)
-- (xs ++ ys) ++ zs

-- Case 2: xs = (a:as)
-- (a:as) ++ (ys ++ zs)
-- -------------------- definition of (++)
-- a : (as ++ (ys ++ zs))
-- a : (as ++ ys ++ zs)
-- --------------------- definition of (++)
-- a : ((as ++ ys) ++ zs)
-- (a:as) ++ ys ++ zs
-- ------------------- definition of (++)
-- ((a:as) ++ ys) ++ zs