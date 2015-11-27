module Codewars.Kata.Capitals where

import Data.Char
import Data.List

capitals :: String -> [Int]
capitals [] = error "todo: capitals"
capitals str = elemIndices True [isUpper x | x <- str]
