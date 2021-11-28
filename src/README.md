##  Trabalho Prático PFL 1

### Primeira parte

#### [Fib.hs](Fib.hs)

##### 1.1)
Uma função recursiva, fibRec;
O tipo desta função é: fibRec :: (Integral a) => a -> a
```
fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n-2) +fibRec (n-1)
```

##### 1.2)
Crie uma versão otimizada da função anterior, chamada fibLista, usando uma lista de
resultados parciais tal que (lista !! i) contém o número de Fibonacci de ordem i
(programação dinâmica).
```
fibLista :: Num p => Int -> p
fibLista 0 = 0
fibLista 1 = 1
fibLista n  = fibs !! (n-1) + fibs !! (n-2) where
  fibs = map fibLista [0..]
```

##### 1.3)
Implemente outra versão, chamada fibListaInfinita, para gerar uma lista infinita com
todos os números de Fibonacci e retornar o elemento de ordem n.
```
fibListaInfinita :: Num a => Int -> a
fibListaInfinita n = listaInfinita !! n
    where listaInfinita  = 0 : 1 : [a + b| (a,b)<- zip listaInfinita (tail listaInfinita)] --lista infinita com todos os números de Fibonacci
```

### Segunda parte

#### [BigNumber.hs](BigNumber.hs)

Deveriamos implementar um módulo para aritmética para big-numbers, que posteriormente seria utilizado para implementar novas versões das funções da alínea 1.

Um big-number é representado por uma lista dos seus dígitos, por isso decidimos implementar um type BigNumber = [Int].



##### 2.1)
Uma definição do tipo BigNumber.
```
module BigNumber where

type BigNumber = [Int]
```
##### 2.2
A função scanner, que converte uma string em big-number. O seu tipo é: String ->
BigNumber
```
-- Converte string para BigNumber
scann :: String  -> BigNumber
scann n = map(`mod` 10) $ reverse $ takeWhile (> 0) $ iterate (`div`10) (read n::Int)
```

##### 2.3
A função output, que converte um big-number em string. O seu tipo é: BigNumber ->
String
```
-- Converte BigNumber para numeros inteiros
dec2int :: [Int] -> Int
dec2int = foldl(\x y -> 10 * x + y) 0

-- Converte BigNumber para String
output :: BigNumber -> String
output xs = show (dec2int xs)
```

##### 2.4) A função somaBN, para somar dois big-numbers.

```
-- Soma de dois BigNumber
somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN bn1 bn2
    | not (checkIfNegative bn1) && not (checkIfNegative bn2) = reverse (somaPosPos (reverse bn1) (reverse bn2) 0 [])  -- 2 positivos
    | checkIfNegative bn1 && checkIfNegative bn2 = changeSign (reverse (somaPosPos (reverse (changeSign bn1)) (reverse (changeSign bn2)) 0 []))  -- 2 negativos
    | absBiggerThan bn1 bn2 && not (checkIfNegative bn1) = removeLeadingZeros (reverse (subPosPos (reverse bn1) (reverse (changeSign bn2)) 0 [])) -- positivo + negativo, abs(positivo) > abs(negativo)
    | absBiggerThan bn2 bn1 && not (checkIfNegative bn1) = changeSign (removeLeadingZeros (reverse (subPosPos (reverse (changeSign bn2)) (reverse bn1) 0 []))) -- positivo + negativo, abs(positivo) < abs(negativo)
    | absBiggerThan bn1 bn2 && checkIfNegative bn1 = changeSign ( removeLeadingZeros(reverse (subPosPos (reverse (changeSign bn1)) (reverse bn2) 0 []))) -- negativo + positivo, abs(negativo) > abs(positivo)
    | otherwise = removeLeadingZeros (reverse (subPosPos (reverse bn2) (reverse (changeSign bn1)) 0 [])) -- negativo + positivo, abs(negativo) < abs(positivo)

```
##### 2.5) A função subBN, para subtrair dois big-numbers.

```
-- Subtraçao entre dois BigNumber
subBN :: BigNumber -> BigNumber -> BigNumber
subBN bn1 bn2 = somaBN bn1 (changeSign bn2)
```
##### 2.6) A função mulBN, para multiplicar dois big-numbers.

```
-- Multiplicaçao de dois BigNumber
multBN :: BigNumber -> BigNumber -> BigNumber
multBN xs ys
    | checkIfNegative xs && checkIfNegative ys = auxMult (map sumMy (multiplyElements ys xs)) [0]
    | not (checkIfNegative xs) && not (checkIfNegative ys) = auxMult (map sumMy (multiplyElements ys xs)) [0]
    | checkIfNegative xs && not (checkIfNegative ys) = changeSign (removeLeadingZeros (auxMult (map sumMy (multiplyElements ys (changeSign xs))) [0]))
    | otherwise = changeSign (removeLeadingZeros (auxMult (map sumMy (multiplyElements (changeSign ys) xs)) [0]))

```

##### 2.7) A função divBN, para efetuar a divisão inteira de dois big-numbers. 

```
-- Divisao de dois BigNumber
divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN bn1 [0]
    = ([-1], [-1])      -- Divisor cannot be 0
divBN bn1 bn2
    | not (checkIfNegative bn1) && not (checkIfNegative bn2) = divPosPos bn1 bn2 [0]
    | checkIfNegative bn1 && checkIfNegative bn2 = divNegNeg bn1 bn2 [0]
    | not (checkIfNegative bn1) && checkIfNegative bn2 = divPosNeg bn1 bn2 [0]
    | otherwise  = divNegPos bn1 bn2 [0]
```

##### 3)

versão das três funções da alínea 1 que trabalhe com big-numbers.

```
```