module Mingle where

(++/) :: [a] -> [a] -> [a]
[] ++/ ys     = ys
(x:xs) ++/ ys = x:(ys ++/ xs)

infixr 5 ++/

-- | The 'mingle' function takes two lists and returns a list of
-- Manual reduction of the following definition:
-- [1,2,3] ++/ [4,5]
-- 1 : ([4,5] ++/ [2,3])
-- 1 : (4 : ([2,3] ++/ [5]))
-- 1 : (4 : (2 : ([5] ++/ [3])))
-- 1 : (4 : (2 : (5 : ([3] ++/ []))))
-- 1 : (4 : (2 : (5 : [3])))
-- 1 : (4 : (2 : [5,3]))
-- 1 : (4 : [2,5,3])
-- 1 : [4,2,5,3]
-- [1,4,2,5,3]

