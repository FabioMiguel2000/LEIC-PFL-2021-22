-- 2.1 [x^2 | x<-[1..100]]

-- 2.2

aprox :: Int->Double 
aprox n = sum [((-1)^x)/fromIntegral(2*x+1) | x<-[0..n]]*4

aprox' :: Int->Double 
aprox' n = sqrt(sum [((-1)^x)/fromIntegral((x+1)^2) | x<-[0..n]]*12)

-- 2.3
dotprod :: [Float] -> [Float] -> Float 
dotprod a b = sum (map times (zip a b))
              where times (x,y) = x * y

dotprod' :: [Float] -> [Float] -> Float 
dotprod' a b = sum[x*y | (x,y) <- zip a b ]

-- 2.4
divdrop :: Integer -> [Integer]
divdrop a = [x | x <-[1..a], a `mod`x==0, x /= a]

-- 2.6
pitagoricos :: Integer -> [(Integer, Integer, Integer)]
pitagoricos n = [(a, b, c) | a<-[1..n],b<-[1..n], c<-[1..n], a^2 == b^2 + c^2]



-- 2.10 d)
myreplicate :: Int -> a -> [a]
myreplicate 0 _:: []
myreplicate n a :: a : myreplicate (n-1) a

-- 2.10 e)
nth0 :: [a] -> Int -> a
nth0 (x:xs) 0 = x
nth0 (x:xs ) n = nth0 xs (n-1)

-- 2.16 e)
alg ∶∶ Int -> [Int]
alg n
    | n < 10 = [n]
    | otherwise = (mod n 10) : algorismos (div n 10)

algorismo :: Int -> [Int]
algorismo n = reverse (alg n)
-- 2.17)
cb ∶∶ Int -> [Int]
cb n b
     | n < b = [n]
     | otherwise = (mod n b) : cb (div n b)

toBits ∶∶ Int → [Int]
toBits n b = reverse (cb n b)

{- Exercício 2.23
   Somar e multiplicar polinómios representados
  como listas de coeficientes.

  Exemplo
   1+2X-X² ~ [1, 2, -1]
-}

-- somar polinómios; versão usando listas em compreensão
addPoly' :: [Int] -> [Int] -> [Int]
addPoly' ps qs
  | n<=m =  -- qs é maior ou igual que ps
     [p+q | (p,q) <- zip (ps ++ replicate (m-n) 0) qs]
  | otherwise = -- ps é maior que qs
      [p+q | (p,q) <- zip ps (qs ++ replicate (n-m) 0) ]
  where
    n = length ps
    m = length qs

-- somar polinómios: versão usando recursão
addPoly :: [Int] -> [Int] -> [Int]
addPoly (p:ps) (q:qs) = (p+q):addPoly ps qs
addPoly []      qs    = qs
addPoly ps      []    = ps


{-
 Multiplicar polinómios
 Ideia do algoritmo recursivo:
 Se
  P = a0 + X*P'
  Q = b0 + X*Q'
 então
  P*Q = a0*b0 + a0*X*Q' + b0*X*P' + X²*P*Q
      = a0*b0 + X*(a0*Q' + b0*P' + X*P*Q)
 -}

multPoly :: [Int] -> [Int] -> [Int]
multPoly []      qs      = []
multPoly ps      []      = []
multPoly (a0:ps) (b0:qs) = (a0*b0) : restantes
  where
    restantes = addPoly (addPoly resto1 resto2) resto3
    resto1 = [a0*q | q<-qs]
    resto2 = [b0*p | p<-ps]
    resto3 = 0 : multPoly ps qs