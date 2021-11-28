import GHC.Real (infinity)
-- import Graphics.Rendering.OpenGL (rescaleNormal)
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

auxSoma :: BigNumbers -> BigNumbers -> BigNumbers
auxSoma xs ys = zipWith (+) xs ys

auxLenght :: BigNumbers -> Int
auxLenght xs = length xs

-- somaBN :: BigNumbers -> BigNumbers -> BigNumbers
-- somaBN bn1 bn2 = revertMakeNeg (reverse (sumWithCarry (reverse (ifNegMakeNeg bn1)) (reverse (ifNegMakeNeg bn2)) 0 []))

somaBN :: BigNumbers -> BigNumbers -> BigNumbers
somaBN bn1 bn2
    | not (checkIfNegative bn1) && not (checkIfNegative bn2) = reverse (somaPosPos (reverse bn1) (reverse bn2) 0 [])  -- 2 positivos
    | checkIfNegative bn1 && checkIfNegative bn2 = changeSign (reverse (somaPosPos (reverse (changeSign bn1)) (reverse (changeSign bn2)) 0 []))  -- 2 negativos
    | absBiggerThan bn1 bn2 && not (checkIfNegative bn1) = removeLeadingZeros (reverse (subPosPos (reverse bn1) (reverse (changeSign bn2)) 0 [])) -- positivo + negativo, abs(positivo) > abs(negativo)
    | absBiggerThan bn2 bn1 && not (checkIfNegative bn1) = changeSign (removeLeadingZeros (reverse (subPosPos (reverse (changeSign bn2)) (reverse bn1) 0 []))) -- positivo + negativo, abs(positivo) < abs(negativo)
    | absBiggerThan bn1 bn2 && checkIfNegative bn1 = changeSign ( removeLeadingZeros(reverse (subPosPos (reverse (changeSign bn1)) (reverse bn2) 0 []))) -- negativo + positivo, abs(negativo) > abs(positivo)
    | otherwise = removeLeadingZeros (reverse (subPosPos (reverse bn2) (reverse (changeSign bn1)) 0 [])) -- negativo + positivo, abs(negativo) < abs(positivo)



subBN :: BigNumbers -> BigNumbers -> BigNumbers
subBN bn1 bn2 = somaBN bn1 (changeSign bn2)

-- multBN :: BigNumbers -> BigNumbers -> BigNumbers
-- multBN bn1 bn2 = revertMakeNeg (reverse (multWithCarry (reverse (ifNegMakeNeg bn1)) (reverse (ifNegMakeNeg bn2)) 0 []))

divBN :: BigNumbers -> BigNumbers -> (BigNumbers, BigNumbers)
divBN bn1 [0] 
    = ([-1], [-1])      -- Divisor cannot be 0
divBN bn1 bn2 
    | not (checkIfNegative bn1) && not (checkIfNegative bn2) = divPosPos bn1 bn2 [0] [0]
    | checkIfNegative bn1 && checkIfNegative bn2 = divNegNeg bn1 bn2 [0] [0]
    | not (checkIfNegative bn1) && checkIfNegative bn2 = divPosPos (changeSign bn1) (changeSign bn2) [0] [0]
    | otherwise  = divPosPos (changeSign bn1) (changeSign bn2) [0] [0]


--     where
--     x = read(output xs)::Int
--     y = read(output ys)::Int

-- bigNumbersToInt :: BigNumbers -> Int
-- bigNumbersToInt bn = foldl (\ x y -> x * 10 + y) 0 bn


-- remainder = dividend - (divisor * quotient)
divPosPos :: BigNumbers -> BigNumbers -> BigNumbers -> BigNumbers -> (BigNumbers, BigNumbers)
divPosPos bn1 bn2 quotient remainder          
    | checkIfNegative (subBN bn1 bn2) = (quotient, bn1)
    | otherwise = divPosPos (subBN bn1 bn2) bn2 (somaBN quotient [1]) remainder

divNegNeg :: BigNumbers -> BigNumbers -> BigNumbers -> BigNumbers -> (BigNumbers, BigNumbers)
divNegNeg bn1 bn2 quotient remainder          
    | not (checkIfNegative (subBN bn1 bn2)) = (quotient, bn1)
    | otherwise = divNegNeg (subBN bn1 bn2) bn2 (somaBN quotient [1]) remainder




-- Aux functions

-- Soma de dois positivos
removeLeadingZeros :: BigNumbers -> BigNumbers
removeLeadingZeros [0]
    = [0]
removeLeadingZeros []
    = []
removeLeadingZeros (x:xs) 
    | x == 0 = removeLeadingZeros xs
    | otherwise = x : xs

somaPosPos :: BigNumbers -> BigNumbers -> Int -> BigNumbers  -> BigNumbers
somaPosPos [] (x:xs) carry res
    = sumWithCarry [] xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` 10])
somaPosPos (x:xs) [] carry res
    = sumWithCarry [] xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` 10])
somaPosPos [] [] carry res
    | carry ==  0 = res
    | otherwise = res ++ [carry]
somaPosPos (x1:xs1) (x2:xs2) carry res
    = sumWithCarry xs1 xs2 ((x1 + x2 + carry) `quot` 10) (res ++ [(x1 + x2 + carry) `mod` 10])


subPosPos :: BigNumbers -> BigNumbers -> Int -> BigNumbers  -> BigNumbers
subPosPos [] [] borrow res
    = res
subPosPos (x:xs) [] borrow res
    | (x - borrow) < 0 = subPosPos xs [] 1 (res ++ [x + 10 - borrow])
    | otherwise = subPosPos xs [] 0 (res ++ [x - borrow])
subPosPos (x1:xs1) (x2:xs2) borrow res
    | (x1 - (borrow + x2)) < 0 = subPosPos xs1 xs2 1 (res ++ [ x1 + 10 - (borrow + x2)])
    | otherwise = subPosPos xs1 xs2 0 (res ++ [ x1 - (borrow + x2)])

absBiggerThan :: BigNumbers -> BigNumbers -> Bool
absBiggerThan [] [] = False
absBiggerThan (x1:xs1) (x2:xs2)
    | length (x1 : xs1) /= length (x2 : xs2) = length (x1 : xs1) > length (x2 : xs2)
    | abs x1 /= abs x2 = abs x1 > abs x2
    | otherwise = absBiggerThan xs1 xs2


checkIfNegative:: BigNumbers -> Bool
checkIfNegative bn = head bn < 0


changeSign :: BigNumbers -> BigNumbers
changeSign [] = []
changeSign (x:xs) = (x*(-1)) : xs


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
sumWithCarry [] (x:xs) carry res
    | (x + carry) < 0 = sumWithCarry [] xs ((x + carry) `quot` 10) (res ++ [ (x+ carry) `mod` (-10)])
    | otherwise = sumWithCarry [] xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` 10])
sumWithCarry (x:xs) [] carry res
    | (x + carry) < 0 = sumWithCarry [] xs ((x + carry) `quot` 10) (res ++ [ (x+ carry) `mod` (-10)])
    | otherwise = sumWithCarry [] xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` 10])
sumWithCarry [] [] carry res
    | carry ==  0 = res
    | otherwise = res ++ [carry]
sumWithCarry (x1:xs1) (x2:xs2) carry res
    | (x1 + x2 + carry) < 0 = sumWithCarry xs1 xs2 ((x1 + x2 + carry) `quot` 10) (res ++ [ (x1 + x2 + carry) `mod` (-10)])
    | otherwise = sumWithCarry xs1 xs2 ((x1 + x2 + carry) `quot` 10) (res ++ [(x1 + x2 + carry) `mod` 10])

sumWithCarrySingleton :: BigNumbers -> Int -> BigNumbers  -> BigNumbers
sumWithCarrySingleton [] carry res
    | carry == 0 = res
    | otherwise = res ++ [carry]
sumWithCarrySingleton (x:xs) carry res
    | (x + carry) < 0 = sumWithCarrySingleton xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` (-10)])
    | otherwise  =  sumWithCarrySingleton xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` 10])

-- subWithCarry :: BigNumbers -> BigNumbers -> Int -> BigNumbers  -> BigNumbers
-- subWithCarry [] (x:xs) carry res
--     | (carry - x) < 0 = subWithCarry [] xs ((carry - x) `quot` 10) (res ++ [ ((carry - x) `mod` (-10))])
--     | otherwise = subWithCarry [] xs ((carry - x) `quot` 10) (res ++ [(carry - x) `mod` 10])
-- subWithCarry (x:xs) [] carry res
--     | (carry - x) < 0 = subWithCarry [] xs ((carry - x) `quot` 10) (res ++ [ ((carry - x) `mod` (-10))])
--     | otherwise = subWithCarry [] xs ((carry - x) `quot` 10) (res ++ [(carry - x) `mod` 10])
-- subWithCarry [] [] carry res
--     | carry ==  0 = res
--     | otherwise = res ++ [carry]
-- subWithCarry (x1:xs1) (x2:xs2) carry res
--     | (x1 - x2 + carry) < 0 = subWithCarry xs1 xs2 ((x1 - x2 + carry) `quot` 10) (res ++ [ ((x1 - x2 + carry) `mod` (-10))])
--     | otherwise = subWithCarry xs1 xs2 ((x1 - x2 + carry) `quot` 10) (res ++ [(x1 - x2 + carry) `mod` 10])


multiplyElements :: BigNumbers -> BigNumbers -> [BigNumbers]
multiplyElements = flip (map . flip (map . (*)))
