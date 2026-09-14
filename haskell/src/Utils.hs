module Utils where

quicksortAsc :: Ord a => [a] -> [a]
quicksortAsc [] = []
quicksortAsc (x:xs) = quicksortAsc lesser ++ [x] ++ quicksortAsc greater
    where lesser = [y | y <- xs, y < x]
          greater = [y | y <- xs, y >= x]

quicksortDes :: Ord a => [a] -> [a]
quicksortDes x = reverse (quicksortAsc x)

inverse :: Num a => [a] -> [a]
inverse = map negate