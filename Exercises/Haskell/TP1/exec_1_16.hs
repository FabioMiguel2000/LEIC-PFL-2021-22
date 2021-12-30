{-
  Defina uma função textual :: Int -> String para
  converter um número positivo inferior a um milhão para
  a designação textual em Português.

  Baseado na solução no livro "Introduction to Functional Programming"
  de Bird & Wadler, 1988.

  Vamos começar por resolver um problema mais pequeno:
  converter para texto números inferiores a 100
  (i.e., até dois algarismos).
  Começamos por definir listas com a representação textual
  de numeros pequenos.
 -}
 
unidades :: [String]
unidades =
  [ "zero"
  , "um"
  , "dois"
  , "tres"
  , "quatro"
  , "cinco"
  , "seis"
  , "sete"
  , "oito"
  , "nove"
  ]

dez_a_dezanove :: [String]
dez_a_dezanove =
  [ "dez"
  , "onze"
  , "doze"
  , "treze"
  , "quatorze"
  , "quinze"
  , "dezasseis"
  , "dezassete"
  , "dezoito"
  , "dezanove"
  ]

dezenas :: [String]
dezenas =
  [ "vinte"
  , "trinta"
  , "quarenta"
  , "cinquenta"
  , "sessenta"
  , "setenta"
  , "oitenta"
  , "noventa"
  ]

{-
  A função 'converte2' é composição de duas:
  * 'divide2' obtêm os algarimos;
  * 'combina2' combina o texto de cada algarismo.
  Usamos as operações de concatenação (++) e
  indexação de listas (!!) (note que os índices começam em zero.)
-}
converte2 :: Int -> String
converte2 n | n<100 = combina2 (divide2 n)

divide2 :: Int -> (Int, Int)
divide2 n = (n`div`10, n`mod`10) -- (quociente,resto)

combina2 :: (Int, Int) -> String
combina2 (0, u) = unidades !! u
combina2 (1, u) = dez_a_dezanove !! u
combina2 (d, 0) = dezenas !! (d-2)
combina2 (d, u) = dezenas !! (d-2) ++ " e " ++ unidades !! u

{- Em seguida, resolvemos o problema análogo para números até 3
   algarismos. Necessitamos dos nomes em Português das centenas.
 -}
centenas :: [String]
centenas =
  [ "cento"
  , "duzentos"
  , "trezentos"
  , "quatrocentos"
  , "quinhentos"
  , "seiscentos"
  , "setecentos"
  , "oitocentos"
  , "novecentos"
  ]

{- A função de conversão, nos mesmos moldes da anterior.
   Note o tratamento especial do número 100.  -}
converte3 :: Int -> String
converte3 n | n<1000 = combina3 (divide3 n)

divide3 :: Int -> (Int, Int)
divide3 n = (n`div`100, n`mod`100)

combina3 :: (Int, Int) -> String
combina3 (0, n) = converte2 n
combina3 (1, 0) = "cem"
combina3 (c, 0) = centenas !! (c-1)
combina3 (c, n) = centenas !! (c-1) ++ " e " ++ converte2 n

{- Finalmente podemos resolver o problema para números
  até 6 algarismos, i.e. inferiores a 1 milhão.  -}
converte6 :: Int -> String
converte6 n | n<1000000 = combina6 (divide6 n)

divide6 n = (n `div` 1000, n `mod` 1000)

combina6 (0, n) = converte3 n
combina6 (1, 0) = "mil"
combina6 (1, n) = "mil" ++ ligar n ++ converte3 n
combina6 (m, 0) = converte3 m ++ " mil"
combina6 (m, n) = converte3 m ++ " mil" ++ ligar n ++ converte3 n

{- Uma função auxiliar para escolher a partícula de ligação entre
   milhares e o restante (r).
   Regra: colocamos "e" quando o resto é inferior a 100
   ou múltiplo de 100; caso contrario, basta um espaço.
 -}
ligar :: Int -> String
ligar r
  | r < 100 || r `mod` 100 == 0 = " e "
  | otherwise                   = " "

-- A solução do exercício proposto é converte6.
converte :: Int -> String
converte = converte6