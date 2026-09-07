module Math.Combinatorics where

factorial :: Integer -> Integer
factorial n
  | n < 0     = error "factorial: negative input"
  | otherwise = product [1..n]

combination :: Integer -> Integer -> Integer
combination n r
    | r < 0 || r > n = 0
    | otherwise      = permutation n r `div` factorial r

permutation :: Integer -> Integer -> Integer
permutation n r
    | r < 0     = 0
    | otherwise = product [n - r + 1..n]

permutationRepetition :: Integer -> Integer -> Integer
permutationRepetition n k
    | k < 0     = error "permutationRepetition: negative input"
    | otherwise = n ^ k

combinationRepetition :: Integer -> Integer -> Integer
combinationRepetition n k
  | k < 0     = error "combinationRepetition: negative input"
  | n == 0    = if k == 0 then 1 else 0
  | otherwise = combination (n + k - 1) k


permutationSlow :: Integer -> Integer -> Integer
permutationSlow n r
    | r < 0     = 0
    | otherwise = factorial n `div` factorial (n - r)

combinationSlow :: Integer -> Integer -> Integer
combinationSlow n r
    | r < 0 || r > n = 0
    | otherwise = factorial n `div` (factorial r * factorial (n - r))