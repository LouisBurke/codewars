module FrequencyFrenzy where
data ToinCoss = Head | Tails | Edge deriving (Eq, Ord, Show)

count :: Ord a => [a] -> a -> Int
count x y = length $ filter (==y) x

frequency :: Ord a => [a] -> [(a, Int)]
frequency [] = []
frequency x = [((head x), count x (head x))] ++ frequency (filter (/=(head x)) x)
