module Fib where

import BigNumbers
import Distribution.Simple.Utils (xargs)

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
fibLista :: Num p => Int -> p
fibLista 0 = 0
fibLista 1 = 1
fibLista n  = (fibs !! (n-1)) + (fibs !! (n-2)) where
  fibs = map fibLista [0..]

{-
Alinea 1.3  Fibonacci Lista Infinita
-}
fibListaInfinita :: Num a => Int -> a
fibListaInfinita n = listaInfinita !! n
    where listaInfinita  = 0 : 1 : [a + b| (a,b)<- zip listaInfinita (tail listaInfinita)] --lista infinita com todos os números de Fibonacci
--fib n = take n . map head $ iterate (\(x:y:xs) -> (x+y):x:xs) [0,1]

{-
Alínea 3 Fibonacci Recursive BigNumbers
-}
fibRecBN ::Int -> BigNumbers 
fibRecBN 0 = [0]
fibRecBN 1 = [1]
fibRecBN n = somaBN (fibRecBN (n-2))  (fibRecBN (n-1))

{-
Alínea 3 Fibonacci Programacao Dinamica
-}
fibListaBN :: Int -> BigNumbers 
fibListaBN 0 = [0]
fibListaBN 1 = [1]
fibListaBN n  = somaBN (fibs !! (n-1)) (fibs !! (n-2)) where
  fibs = map fibListaBN [0..]

{-
Alínea 3 Fibonacci Lista Infinita
-}
fibListaInfinitaBN :: Int -> BigNumbers 
fibListaInfinitaBN n = scann (show (listaInfinita !! n))
    where listaInfinita  = 0 : 1 : [a + b| (a,b)<- zip listaInfinita (tail listaInfinita)] 
