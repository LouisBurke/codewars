module NumberSet(numberSet, isVampire) where
import Data.Set

numberSet :: Integer -> Set Char
numberSet x = fromList (show x)

isVampire :: Integer -> Integer -> Bool
isVampire a b =
  (unions [(numberSet a), (numberSet b)]) == (numberSet $ a*b)
