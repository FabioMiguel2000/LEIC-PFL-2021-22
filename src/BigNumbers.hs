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

somaBN :: BigNumbers -> BigNumbers -> BigNumbers
somaBN xs ys = scann (show (sum [x + y | (x,y) <- zip xs ys]))

subBN :: BigNumbers -> BigNumbers -> BigNumbers
subBN xs ys = scann (show (sum [x - y | (x,y) <- zip xs ys]))

multBN :: BigNumbers -> BigNumbers -> BigNumbers
multBN xs ys = scann (show (sum [x * y | (x,y) <- zip xs ys]))

divBN :: BigNumbers -> BigNumbers -> (BigNumbers, BigNumbers)
divBN xs ys = (scann (show (sum [mod x y | (x,y) <- zip xs ys])),scann (show (sum [x `div` y | (x,y) <- zip xs ys])))