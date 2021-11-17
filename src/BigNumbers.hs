type BigNumbers = [Int]

-- instance Show BigNumbers where
 --   show (BigNumbers n) = show (n)

scann :: String  -> BigNumbers
scann n = map(`mod` 10) $ reverse $ takeWhile (> 0) $ iterate (`div`10) (read n::Int)
-- digits n = map (\x -> read [x] :: Int) (show n)