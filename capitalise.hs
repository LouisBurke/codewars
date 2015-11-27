module JadenCasing where
import Data.List
import Data.Char

capFirst :: String -> String
capFirst (x:xs) = (toUpper x) : xs

toJadenCase :: [String] -> String
toJadenCase (words js) = capFirst (head js) ++ 
