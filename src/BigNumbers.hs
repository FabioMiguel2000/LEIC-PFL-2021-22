
module BigNumbers where

type BigNumbers = [Int]

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

-- Soma de dois BigNumbers
somaBN :: BigNumbers -> BigNumbers -> BigNumbers
somaBN bn1 bn2
    | not (checkIfNegative bn1) && not (checkIfNegative bn2) = reverse (somaPosPos (reverse bn1) (reverse bn2) 0 [])  -- 2 positivos
    | checkIfNegative bn1 && checkIfNegative bn2 = changeSign (reverse (somaPosPos (reverse (changeSign bn1)) (reverse (changeSign bn2)) 0 []))  -- 2 negativos
    | absBiggerThan bn1 bn2 && not (checkIfNegative bn1) = removeLeadingZeros (reverse (subPosPos (reverse bn1) (reverse (changeSign bn2)) 0 [])) -- positivo + negativo, abs(positivo) > abs(negativo)
    | absBiggerThan bn2 bn1 && not (checkIfNegative bn1) = changeSign (removeLeadingZeros (reverse (subPosPos (reverse (changeSign bn2)) (reverse bn1) 0 []))) -- positivo + negativo, abs(positivo) < abs(negativo)
    | absBiggerThan bn1 bn2 && checkIfNegative bn1 = changeSign ( removeLeadingZeros(reverse (subPosPos (reverse (changeSign bn1)) (reverse bn2) 0 []))) -- negativo + positivo, abs(negativo) > abs(positivo)
    | otherwise = removeLeadingZeros (reverse (subPosPos (reverse bn2) (reverse (changeSign bn1)) 0 [])) -- negativo + positivo, abs(negativo) < abs(positivo)

-- Subtraçao entre dois BigNumbers
subBN :: BigNumbers -> BigNumbers -> BigNumbers
subBN bn1 bn2 = somaBN bn1 (changeSign bn2)

-- Multiplicaçao de dois BigNumbers
multBN :: BigNumbers -> BigNumbers -> BigNumbers
multBN xs ys
    | checkIfNegative xs && checkIfNegative ys = auxMult (map sumMy (multiplyElements ys xs)) [0]
    | not (checkIfNegative xs) && not (checkIfNegative ys) = auxMult (map sumMy (multiplyElements ys xs)) [0]
    | checkIfNegative xs && not (checkIfNegative ys) = changeSign (removeLeadingZeros (auxMult (map sumMy (multiplyElements ys (changeSign xs))) [0]))
    | otherwise = changeSign (removeLeadingZeros (auxMult (map sumMy (multiplyElements (changeSign ys) xs)) [0]))

-- Divisao de dois BigNumbers
divBN :: BigNumbers -> BigNumbers -> (BigNumbers, BigNumbers)
divBN bn1 [0] 
    = ([-1], [-1])      -- Divisor cannot be 0
divBN bn1 bn2 
    | not (checkIfNegative bn1) && not (checkIfNegative bn2) = divPosPos bn1 bn2 [0]
    | checkIfNegative bn1 && checkIfNegative bn2 = divNegNeg bn1 bn2 [0]
    | not (checkIfNegative bn1) && checkIfNegative bn2 = divPosNeg bn1 bn2 [0]
    | otherwise  = divNegPos bn1 bn2 [0]


--     where
--     x = read(output xs)::Int
--     y = read(output ys)::Int

-- bigNumbersToInt :: BigNumbers -> Int
-- bigNumbersToInt bn = foldl (\ x y -> x * 10 + y) 0 bn


----------------------------- Aux functions -----------------------------------------

auxSoma :: BigNumbers -> BigNumbers -> BigNumbers
auxSoma xs ys = zipWith (+) xs ys

auxLenght :: BigNumbers -> Int
auxLenght xs = length xs


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

-- dividend = (divisor * quotient) + remainder
divPosPos :: BigNumbers -> BigNumbers -> BigNumbers -> (BigNumbers, BigNumbers)
divPosPos bn1 bn2 quotient          
    | checkIfNegative (subBN bn1 bn2) = (quotient, bn1)
    | otherwise = divPosPos (subBN bn1 bn2) bn2 (somaBN quotient [1]) 

divNegNeg :: BigNumbers -> BigNumbers -> BigNumbers -> (BigNumbers, BigNumbers)
divNegNeg bn1 bn2 quotient
    | checkIfPositive (subBN bn1 bn2) = (quotient, bn1)
    | otherwise = divNegNeg (subBN bn1 bn2) bn2 (somaBN quotient [1]) 

divNegPos :: BigNumbers -> BigNumbers -> BigNumbers -> (BigNumbers, BigNumbers)
divNegPos bn1 bn2 quotient           
    | checkIfPositive (somaBN bn1 bn2) = (quotient, bn1)
    | otherwise = divNegPos (somaBN bn1 bn2) bn2 (somaBN quotient [-1]) 

divPosNeg :: BigNumbers -> BigNumbers -> BigNumbers -> (BigNumbers, BigNumbers)
divPosNeg bn1 bn2 quotient           
    | checkIfNegative (somaBN bn1 bn2) = (quotient, bn1)
    | otherwise = divPosNeg (somaBN bn1 bn2) bn2 (somaBN quotient [-1]) 

absBiggerThan :: BigNumbers -> BigNumbers -> Bool
absBiggerThan [] [] = False
absBiggerThan (x1:xs1) (x2:xs2)
    | length (x1 : xs1) /= length (x2 : xs2) = length (x1 : xs1) > length (x2 : xs2)
    | abs x1 /= abs x2 = abs x1 > abs x2
    | otherwise = absBiggerThan xs1 xs2


checkIfNegative:: BigNumbers -> Bool
checkIfNegative bn = head bn < 0

checkIfPositive :: BigNumbers -> Bool
checkIfPositive bn = head bn > 0

changeSign :: BigNumbers -> BigNumbers
changeSign [] = []
changeSign (x:xs) = (x*(-1)) : xs

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


-- My idea is to treat the list of lists generated by multiplyElements where:
-- 1: I'll run sumWithCarrySingleton with the reverse of each element of this list of big numbers, changing this item for a new list represented in BigNumber notation.
-- 2: Once ths list of lists is represented in the right bigNumbers notation i'll sum each list with the other, could try to use a curry function somaBN'
-- The result of this Sum would be the result of a multiplication between two bigNumbers ex [2,3] [4,5,6] *The minor value should come first

sumWithCarrySingleton :: BigNumbers -> Int -> BigNumbers  -> BigNumbers
sumWithCarrySingleton [] carry res
    | carry == 0 = res
    | otherwise = res ++ [carry]
sumWithCarrySingleton (x:xs) carry res
    | (x + carry) < 0 = sumWithCarrySingleton xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` (-10)])
    | otherwise  =  sumWithCarrySingleton xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` 10])

multiplyElements :: BigNumbers -> BigNumbers -> [BigNumbers]
multiplyElements = flip (map . flip (map . (*)))

sumMy :: BigNumbers -> BigNumbers
sumMy xs = reverse(sumWithCarrySingleton (reverse(xs)) 0 [])

multBN :: BigNumbers -> BigNumbers -> BigNumbers
multBN xs ys
    | checkIfNegative xs && checkIfNegative ys = auxMult (map sumMy (multiplyElements ys xs)) [0]
    | not (checkIfNegative xs) && not (checkIfNegative ys) = auxMult (map sumMy (multiplyElements ys xs)) [0]
    | checkIfNegative xs && not (checkIfNegative ys) = changeSign (removeLeadingZeros (auxMult (map sumMy (multiplyElements ys (changeSign xs))) [0]))
    | otherwise = changeSign (removeLeadingZeros (auxMult (map sumMy (multiplyElements (changeSign ys) xs)) [0]))

auxMult :: [BigNumbers] -> BigNumbers -> BigNumbers
auxMult [] res = res
auxMult (x:xs) res = auxMult xs (multiSumTwo x res)


-- If the diference between the lists to be summed is equal 1:
-- (somaBN (reverse(drop 1 (reverse[1,3,6,8 ]))) (reverse [2,1,9]))++[(head(reverse [1,3,6,8]))]

--If the diference between the lists to be summed is equal to 2:
-- [head(reverse[2,1,3,1])]++(somaBN (reverse(drop 1 (reverse[1,3,6,8 ]))) (drop 1 (reverse [2,1,3,1])))++[(head(reverse [1,3,6,8]))]8

multiSumTwo :: BigNumbers -> BigNumbers -> BigNumbers
multiSumTwo l1 l2 
    | ((length (l1))-(length (l2)) > 0) = (somaBN (reverse(drop 1(reverse(l1)))) (l2)) ++ [head(reverse(l1))]
    | ((length (l1))-(length (l2)) < 0) = (somaBN (0:(0:(reverse(drop 1(reverse(l1)))))) (l2)) ++ [head(reverse(l1))]
    | otherwise = (somaBN (0:(reverse(drop 1(reverse(l1))))) (l2)) ++ [head(reverse(l1))]

