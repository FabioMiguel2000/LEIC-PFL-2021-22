type BigNumbers = [Int]

-- data BigNumbers = BigNumbers [Int]

-- instance Show BigNumbers where
 --   show (BigNumbers n) = show (n)

-- Converte string para BigNumbers
scann :: String  -> BigNumbers
scann n = map(`mod` 10) $ reverse $ takeWhile (> 0) $ iterate (`div`10) (read n::Int)

-- Converte BigNumbers para numeros inteiros
dec2int :: [Int] -> Int
dec2int a = foldl(\x y -> 10 * x + y) 0 a

-- Converte BigNumbers para String
output :: BigNumbers -> String
output xs = show (dec2int xs)

auxLenght :: BigNumbers -> Int 
auxLenght xs = length xs

somaBN :: BigNumbers -> BigNumbers -> BigNumbers
somaBN bn1 bn2 = revertMakeNeg (reverse (sumWithCarry (reverse (ifNegMakeNeg bn1)) (reverse (ifNegMakeNeg bn2)) 0 []))

subBN :: BigNumbers -> BigNumbers -> BigNumbers
subBN bn1 bn2 = revertMakeNeg (reverse (subWithCarry (reverse (ifNegMakeNeg bn1)) (reverse (ifNegMakeNeg bn2)) 0 []))

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

revertMakeNeg :: BigNumbers -> BigNumbers
revertMakeNeg (x:xs) 
    | x < 0 = x : map abs xs
    | otherwise = x:xs

sumWithCarry :: BigNumbers -> BigNumbers -> Int -> BigNumbers  -> BigNumbers
sumWithCarry [] [] carry res 
    | carry ==  0 = res
    | otherwise = res ++ [carry]
sumWithCarry (x1:xs1) (x2:xs2) carry res 
    | (x1 + x2 + carry) < 0 = sumWithCarry xs1 xs2 ((x1 + x2 + carry) `quot` 10) (res ++ [ ((x1 + x2 + carry) `mod` (-10))])
    | otherwise = sumWithCarry xs1 xs2 ((x1 + x2 + carry) `quot` 10) (res ++ [(x1 + x2 + carry) `mod` 10])

sumWithCarrySingleton :: BigNumbers -> Int -> BigNumbers  -> BigNumbers
sumWithCarrySingleton [] carry res
    | carry == 0 = res
    | otherwise = res ++ [carry]
sumWithCarrySingleton (x:xs) carry res
    | (x + carry) < 0 = sumWithCarrySingleton xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` (-10)])
    | otherwise  =  sumWithCarrySingleton xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` 10])

subWithCarry [] [] carry res 
    | carry ==  0 = res
    | otherwise = res ++ [carry]
subWithCarry (x1:xs1) (x2:xs2) carry res 
    | (x1 - x2 + carry) < 0 = subWithCarry xs1 xs2 ((x1 - x2 + carry) `quot` 10) (res ++ [ ((x1 - x2 + carry) `mod` (-10))])
    | otherwise = subWithCarry xs1 xs2 ((x1 - x2 + carry) `quot` 10) (res ++ [(x1 - x2 + carry) `mod` 10])


-- My idea is to treat the list of lists generated by multiplyElements where:
-- 1: I'll run sumWithCarrySingleton with the reverse of each element of this list of big numbers, changing this item for a new list represented in BigNumber notation.
-- 2: Once ths list of lists is represented in the right bigNumbers notation i'll sum each list with the other, could try to use a curry function somaBN'
-- The result of this Sum would be the result of a multiplication between two bigNumbers ex [2,3] [4,5,6] *The minor value should come first
multTreat :: [BigNumbers] -> [BigNumbers]
multTreat [x] = [sumWithCarrySingleton x 0 [] ]

multiplyElements :: BigNumbers -> BigNumbers -> [BigNumbers]
multiplyElements = flip (map . flip (map . (*)))

-- If the diference between the lists to be summed is equal 1:
-- (somaBN (reverse(drop 1 (reverse[1,3,6,8 ]))) (reverse [2,1,9]))++[(head(reverse [1,3,6,8]))]

--If the diference between the lists to be summed is equal to 2:
-- [head(reverse[2,1,3,1])]++(somaBN (reverse(drop 1 (reverse[1,3,6,8 ]))) (drop 1 (reverse [2,1,3,1])))++[(head(reverse [1,3,6,8]))]8