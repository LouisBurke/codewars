module RowSumOddNumbers(less_mem_take, rowSumOddNumbers) where
import Data.List

less_mem_take :: Integer -> Integer -> [Integer]
less_mem_take len counter
  | len == 0 = []
  | (odd counter) = [counter] ++ less_mem_take (len-1) (counter+1)
  | (even counter) = less_mem_take len (counter+1)

rowSumOddNumbers :: Integer -> Integer
rowSumOddNumbers n = foldr (+) 0 $ drop (length (taken n) - (fromIntegral n)) (taken n)
  where
    --taken n = take (max_needed n) [x | x <- [1..], odd x]
    taken n = less_mem_take (max_needed n) 1
    max_needed n = fromIntegral ((foldr (+) 0 $ reverse [1..(n-1)]) + n)
