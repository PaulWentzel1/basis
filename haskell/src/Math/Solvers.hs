module Math.Solvers where

-- | A basic bisection root finder for an increasing function
biSection' :: (Double -> Double) -> Double -> Double -> Double -> Double
biSection' errorFunc low high threshold
  | high - low < 1e-6 = mid
  | errorFunc mid > 0 = biSection' errorFunc low mid threshold
  | otherwise         = biSection' errorFunc mid high threshold
  where
    mid = (low + high) / 2

-- | Wrapper function for biSection', threshold set to 1e-6
biSection :: (Double -> Double) -> Double -> Double -> Double
biSection errorFunc low high = biSection' errorFunc low high 1e-6
