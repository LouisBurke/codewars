module RowSumOddNumbers where
import Data.List

rowSumOddNumbers :: Integer -> Integer
rowSumOddNumbers n = foldr (+) 0 $ drop (length (taken n) - (fromIntegral n)) (taken n)
  where
    taken n = take (max_needed n) [x | x <- [1..], odd x]
    max_needed n = fromIntegral ((foldr (+) 0 $ reverse [1..(n-1)]) + n)
