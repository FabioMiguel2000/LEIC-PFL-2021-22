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