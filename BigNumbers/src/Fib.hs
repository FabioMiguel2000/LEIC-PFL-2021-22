module Fib where

import BigNumber

{-
Alinea 1.1  Fibonacci Funcao Recursiva
-}
fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n-2) +fibRec (n-1)

{-
Alinea 1.2  Fibonacci Programacao Dinamica
-}
fibLista :: Int -> Int
fibLista 0 = 0
fibLista 1 = 1
fibLista n  = fibs !! (n-1) + fibs !! (n-2) where
  fibs = map fibLista [0..]

{-
Alinea 1.3  Fibonacci Lista Infinita
-}
fibListaInfinita :: Int -> Integer
fibListaInfinita n = listaInfinita !! n
    where listaInfinita  = 0 : 1 : [a + b| (a,b)<- zip listaInfinita (tail listaInfinita)] --lista infinita com todos os números de Fibonacci
--fib n = take n . map head $ iterate (\(x:y:xs) -> (x+y):x:xs) [0,1]


--------------- Big Number ------------------


{-
Alínea 3 Fibonacci Recursive BigNumber
-}
fibRecBN ::BigNumber -> BigNumber
fibRecBN [0] = [0]
fibRecBN [1] = [1]
fibRecBN n = somaBN (fibRecBN (subBN n [2]))  (fibRecBN (subBN n [1]))

{-
Alínea 3 Fibonacci Programacao Dinamica
-}
fibListaBN :: BigNumber -> BigNumber
fibListaBN [0] = [0]
fibListaBN [1] = [1]
fibListaBN n  = somaBN (indexer fibs (subBN n [1])) (indexer fibs (subBN n [2])) where
  fibs = map fibListaBN listBN

{-
Alínea 3 Fibonacci Lista Infinita
-}
fibListaInfinitaBN :: BigNumber -> BigNumber
fibListaInfinitaBN = indexer listaInfinita
    where listaInfinita  = [0] : [1] : [somaBN a  b| (a,b)<- zip listaInfinita (tail listaInfinita)]


--------------------- aux ------------------------

fibBN :: [BigNumber]
fibBN = [0] : [1] : zipWith somaBN fibBN (tail fibBN)

listBN :: [BigNumber]
listBN = iterate (\x -> somaBN x [1]) [0]

indexer :: [BigNumber] -> BigNumber-> BigNumber
indexer (x:xs) [0] = x
indexer (x:xs) n = indexer  xs (subBN n [1])