module BigNumber where

type BigNumber = [Int]

-- Converte string para BigNumber
scann :: String  -> BigNumber
scann n = map(`mod` 10) $ reverse $ takeWhile (> 0) $ iterate (`div`10) (read n::Int)

-- Converte BigNumber para numeros inteiros
dec2int :: [Int] -> Int
dec2int = foldl(\x y -> 10 * x + y) 0

-- Converte BigNumber para String
output :: BigNumber -> String
output xs = show (dec2int xs)

-- Soma de dois BigNumber
somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN bn1 bn2
    | not (checkIfNegative bn1) && not (checkIfNegative bn2) = reverse (somaPosPos (reverse bn1) (reverse bn2) 0 [])  -- 2 positivos
    | checkIfNegative bn1 && checkIfNegative bn2 = changeSign (reverse (somaPosPos (reverse (changeSign bn1)) (reverse (changeSign bn2)) 0 []))  -- 2 negativos
    | absBiggerThan bn1 bn2 && not (checkIfNegative bn1) = removeLeadingZeros (reverse (subPosPos (reverse bn1) (reverse (changeSign bn2)) 0 [])) -- positivo + negativo, abs(positivo) > abs(negativo)
    | absBiggerThan bn2 bn1 && not (checkIfNegative bn1) = changeSign (removeLeadingZeros (reverse (subPosPos (reverse (changeSign bn2)) (reverse bn1) 0 []))) -- positivo + negativo, abs(positivo) < abs(negativo)
    | absBiggerThan bn1 bn2 && checkIfNegative bn1 = changeSign ( removeLeadingZeros(reverse (subPosPos (reverse (changeSign bn1)) (reverse bn2) 0 []))) -- negativo + positivo, abs(negativo) > abs(positivo)
    | otherwise = removeLeadingZeros (reverse (subPosPos (reverse bn2) (reverse (changeSign bn1)) 0 [])) -- negativo + positivo, abs(negativo) < abs(positivo)

-- Subtraçao entre dois BigNumber
subBN :: BigNumber -> BigNumber -> BigNumber
subBN bn1 bn2 = somaBN bn1 (changeSign bn2)

-- Multiplicaçao de dois BigNumber
mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN xs ys
    | checkIfNegative xs && checkIfNegative ys = auxMult (map sumMy (mulItems ys xs)) [0]
    | not (checkIfNegative xs) && not (checkIfNegative ys) = auxMult (map sumMy (mulItems ys xs)) [0]
    | checkIfNegative xs && not (checkIfNegative ys) = changeSign (removeLeadingZeros (auxMult (map sumMy (mulItems ys (changeSign xs))) [0]))
    | otherwise = changeSign (removeLeadingZeros (auxMult (map sumMy (mulItems (changeSign ys) xs)) [0]))

-- Divisao de dois BigNumber
divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN bn1 [0]
    = ([-1], [-1])      -- Divisor cannot be 0
divBN bn1 bn2
    | not (checkIfNegative bn1) && not (checkIfNegative bn2) = divPosPos bn1 bn2 [0]
    | checkIfNegative bn1 && checkIfNegative bn2 = divNegNeg bn1 bn2 [0]
    | not (checkIfNegative bn1) && checkIfNegative bn2 = divPosNeg bn1 bn2 [0]
    | otherwise  = divNegPos bn1 bn2 [0]

safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN bn1 [0]
    = Nothing      -- Divisor cannot be 0
safeDivBN bn1 bn2
    | not (checkIfNegative bn1) && not (checkIfNegative bn2) = Just (divPosPos bn1 bn2 [0])
    | checkIfNegative bn1 && checkIfNegative bn2 = Just (divNegNeg bn1 bn2 [0])
    | not (checkIfNegative bn1) && checkIfNegative bn2 = Just (divPosNeg bn1 bn2 [0])
    | otherwise  = Just (divNegPos bn1 bn2 [0])

--     where
--     x = read(output xs)::Int
--     y = read(output ys)::Int

-- BigNumberToInt :: BigNumber -> Int
-- BigNumberToInt bn = foldl (\ x y -> x * 10 + y) 0 bn


----------------------------- Aux functions -----------------------------------------
removeLeadingZeros :: BigNumber -> BigNumber
removeLeadingZeros [0]
    = [0]
removeLeadingZeros []
    = []
removeLeadingZeros (x:xs)
    | x == 0 = removeLeadingZeros xs
    | otherwise = x : xs

somaPosPos :: BigNumber -> BigNumber -> Int -> BigNumber  -> BigNumber
somaPosPos [] (x:xs) carry res
    = sumWithCarry [] xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` 10])
somaPosPos (x:xs) [] carry res
    = sumWithCarry [] xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` 10])
somaPosPos [] [] carry res
    | carry ==  0 = res
    | otherwise = res ++ [carry]
somaPosPos (x1:xs1) (x2:xs2) carry res
    = sumWithCarry xs1 xs2 ((x1 + x2 + carry) `quot` 10) (res ++ [(x1 + x2 + carry) `mod` 10])

subPosPos :: BigNumber -> BigNumber -> Int -> BigNumber  -> BigNumber
subPosPos [] [] borrow res
    = res
subPosPos (x:xs) [] borrow res
    | x - borrow < 0 = subPosPos xs [] 1 (res ++ [x + 10 - borrow])
    | otherwise = subPosPos xs [] 0 (res ++ [x - borrow])
subPosPos (x1:xs1) (x2:xs2) borrow res
    | x1 - (borrow + x2) < 0 = subPosPos xs1 xs2 1 (res ++ [ x1 + 10 - (borrow + x2)])
    | otherwise = subPosPos xs1 xs2 0 (res ++ [ x1 - (borrow + x2)])

-- dividend = (divisor * quotient) + remainder
divPosPos :: BigNumber -> BigNumber -> BigNumber -> (BigNumber, BigNumber)
divPosPos bn1 bn2 quotient
    | checkIfNegative (subBN bn1 bn2) = (quotient, bn1)
    | otherwise = divPosPos (subBN bn1 bn2) bn2 (somaBN quotient [1])

divNegNeg :: BigNumber -> BigNumber -> BigNumber -> (BigNumber, BigNumber)
divNegNeg bn1 bn2 quotient
    | checkIfPositive (subBN bn1 bn2) = (quotient, bn1)
    | otherwise = divNegNeg (subBN bn1 bn2) bn2 (somaBN quotient [1])

divNegPos :: BigNumber -> BigNumber -> BigNumber -> (BigNumber, BigNumber)
divNegPos bn1 bn2 quotient
    | checkIfPositive (somaBN bn1 bn2) = (quotient, bn1)
    | otherwise = divNegPos (somaBN bn1 bn2) bn2 (somaBN quotient [-1])

divPosNeg :: BigNumber -> BigNumber -> BigNumber -> (BigNumber, BigNumber)
divPosNeg bn1 bn2 quotient
    | checkIfNegative (somaBN bn1 bn2) = (quotient, bn1)
    | otherwise = divPosNeg (somaBN bn1 bn2) bn2 (somaBN quotient [-1])

absBiggerThan :: BigNumber -> BigNumber -> Bool
absBiggerThan [] [] = False
absBiggerThan (x1:xs1) (x2:xs2)
    | length (x1 : xs1) /= length (x2 : xs2) = length (x1 : xs1) > length (x2 : xs2)
    | abs x1 /= abs x2 = abs x1 > abs x2
    | otherwise = absBiggerThan xs1 xs2

checkIfNegative:: BigNumber -> Bool
checkIfNegative bn = head bn < 0

checkIfPositive :: BigNumber -> Bool
checkIfPositive bn = head bn > 0

changeSign :: BigNumber -> BigNumber
changeSign [] = []
changeSign (x:xs) = (x*(-1)) : xs

sumWithCarry :: BigNumber -> BigNumber -> Int -> BigNumber  -> BigNumber
sumWithCarry [] (x:xs) carry res
    | x + carry < 0 = sumWithCarry [] xs ((x + carry) `quot` 10) (res ++ [ (x+ carry) `mod` (-10)])
    | otherwise = sumWithCarry [] xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` 10])
sumWithCarry (x:xs) [] carry res
    | x + carry < 0 = sumWithCarry [] xs ((x + carry) `quot` 10) (res ++ [ (x+ carry) `mod` (-10)])
    | otherwise = sumWithCarry [] xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` 10])
sumWithCarry [] [] carry res
    | carry ==  0 = res
    | otherwise = res ++ [carry]
sumWithCarry (x1:xs1) (x2:xs2) carry res
    | x1 + x2 + carry < 0 = sumWithCarry xs1 xs2 ((x1 + x2 + carry) `quot` 10) (res ++ [ (x1 + x2 + carry) `mod` (-10)])
    | otherwise = sumWithCarry xs1 xs2 ((x1 + x2 + carry) `quot` 10) (res ++ [(x1 + x2 + carry) `mod` 10])

-- My idea is to treat the list of lists generated by mulItems where:
-- 1: I'll run sumWithCarrySingleton with the reverse of each element of this list of big numbers, changing this item for a new list represented in BigNumber notation.
-- 2: Once ths list of lists is represented in the right BigNumber notation i'll sum each list with the other, could try to use a curry function somaBN'
-- The result of this Sum would be the result of a multiplication between two BigNumber ex [2,3] [4,5,6] *The minor value should come first

sumWithCarrySingleton :: BigNumber -> Int -> BigNumber  -> BigNumber
sumWithCarrySingleton [] carry res
    | carry == 0 = res
    | otherwise = res ++ [carry]
sumWithCarrySingleton (x:xs) carry res
    | x + carry < 0 = sumWithCarrySingleton xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` (-10)])
    | otherwise  =  sumWithCarrySingleton xs ((x + carry) `quot` 10) (res ++ [(x + carry) `mod` 10])

mulItems :: BigNumber -> BigNumber -> [BigNumber]
mulItems = flip (map . flip (map . (*)))

sumMy :: BigNumber -> BigNumber
sumMy xs = reverse(sumWithCarrySingleton (reverse xs) 0 [])

auxMult :: [BigNumber] -> BigNumber -> BigNumber
auxMult xs res = foldl (flip multiSumTwo) res xs

multiSumTwo :: BigNumber -> BigNumber -> BigNumber
multiSumTwo l1 l2
    | length l1-length l2 > 0 = somaBN (reverse(drop 1(reverse l1))) l2 ++ [last l1]
    | length l1-length l2 < 0 = somaBN (0:(0:reverse(drop 1(reverse l1)))) l2 ++ [last l1]
    | otherwise = somaBN (0:reverse(drop 1(reverse l1))) l2 ++ [last l1]
