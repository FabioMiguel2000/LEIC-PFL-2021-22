import Data.List ( insert )
-- 3.1
myFunc ::  (a -> a) ->  (a -> Bool) -> [a] -> [a]
myFunc f p x = map f (filter p x)

-- 3.2
dec2int :: [Int] -> Int
dec2int a = foldl(\x y -> 10 * x + y) 0 a

-- 3.3
--zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
--zipWith' a b = zipWith(\x y -> )

-- 3.4
myIsort :: Ord a => [a] -> [a]
myIsort a = foldr (\x y -> insert x y) [] a

-- 3.7

--a) 
(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (:) ys xs

--b)
myConcat :: [[a]] -> [a]
myConcat a = foldr (\x y -> x +++ y) [] a

--c) x é o resto da lista e y é o acumulador
myReverser :: [a] -> [a]
myReverser a = foldr (\x y -> y ++ [x]) [] a

--d) x é o acumulador e y é o resto da lista
myReversel :: [a]-> [a]
myReversel a = foldl (\x y -> y : x) [] a