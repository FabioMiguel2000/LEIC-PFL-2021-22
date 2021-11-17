-- Test if a given edge of a triangule is smaller then the sum of the other two edges
testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = (a+b) > c && (b+c) > a && (a+c) > b

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c =
    if testaTriangulo a b c then
      let s = (a+b+c)/2
      in sqrt(s*(s-a)*(s-b)*(s-c))
    else -1

metades :: [a] -> ([a],[a])
metades l =
  let size = div (length l) 2
  in splitAt size l

-- 1.4 a) last -> reverse l // head l
-- Last ->

-- 1.4 b) init 

-- 1.7
-- a) [char] or string
-- b) (char,char,char)
-- c) [(bool,char)]
-- d) ([bool],[char])
-- e) [a]->[a]
-- f) devido ao not::bool->bool, isso força que  lista seja [bool->bool], apenas o id seria [a->a]

-- 1.8
-- a) [a]->a
-- b) (a,b)->(b,a)
-- c) a -> b -> (a,b)
-- d) Num a => a -> a
-- e) Fraction a => a -> a
-- f) Char -> Bool  
-- g) Ord a=> a -> a -> a -> bool
-- h) Eq a => [a] -> bool
-- i) (a -> a) -> a -> a

ourxor :: Bool -> Bool -> Bool 
ourxor x y = x /= y

theirxor :: Bool -> Bool -> Bool
theirxor True  True = False 
theirxor True False = True 
theirxor False True = True 
theirxor False False = False 