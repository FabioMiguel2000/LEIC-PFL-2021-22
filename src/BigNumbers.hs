type BigNumbers = [Int]

-- data BigNumbers = BigNumbers [Int]

-- instance Show BigNumbers where
 --   show (BigNumbers n) = show (n)

scann :: String  -> BigNumbers
scann n = map(`mod` 10) $ reverse $ takeWhile (> 0) $ iterate (`div`10) (read n::Int)

dec2int :: [Int] -> Int
dec2int a = foldl(\x y -> 10 * x + y) 0 a

output :: BigNumbers -> String
output xs = show (dec2int xs)

auxSoma :: BigNumbers -> BigNumbers -> BigNumbers
auxSoma xs ys = zipWith (+) xs ys

auxLenght :: BigNumbers -> Int 
auxLenght xs = length xs

somaBN :: BigNumbers -> BigNumbers -> BigNumbers
somaBN bn1 bn2 = reverse (sumWithCarry (reverse (ifNegMakeNeg bn1)) (reverse (ifNegMakeNeg bn2)) 0 [])

subBN :: BigNumbers -> BigNumbers -> BigNumbers
subBN bn1 bn2 = reverse (subWithCarry (reverse (ifNegMakeNeg bn1)) (reverse (ifNegMakeNeg bn2)) 0 [])

multBN :: BigNumbers -> BigNumbers -> BigNumbers
multBN xs ys = scann(show(x*y))
    where
    x = read(output xs)::Int
    y = read(output ys)::Int

divBN :: BigNumbers -> BigNumbers -> (BigNumbers, BigNumbers)
divBN xs ys = (scann(show(mod x y)), scann(show(x`div`y)))
    where
    x = read(output xs)::Int
    y = read(output ys)::Int
    
-- Aux functions
makeNeg :: Num a => a -> a
makeNeg n = -1*n

ifNegMakeNeg :: BigNumbers -> BigNumbers
ifNegMakeNeg (x:xs) 
    | x < 0 = x : map makeNeg xs
    | otherwise = x:xs

sumWithCarry :: BigNumbers-> BigNumbers -> Int -> BigNumbers  -> BigNumbers
sumWithCarry [] [] carry res 
    | carry ==  0 = res
    | otherwise = res ++ [carry]
sumWithCarry (x1:xs1) (x2:xs2) carry res  = sumWithCarry xs1 xs2 ((x1 + x2 + carry) `quot` 10) (res ++ [(x1 + x2 + carry) `mod` 10])

subWithCarry :: BigNumbers-> BigNumbers -> Int -> BigNumbers  -> BigNumbers
subWithCarry [] [] carry res 
    | carry ==  0 = res
    | otherwise = res ++ [carry]
subWithCarry (x1:xs1) (x2:xs2) carry res  = subWithCarry xs1 xs2 ((x1 - x2 + carry) `quot` 10) (res ++ [(x1 - x2 + carry) `mod` 10])

