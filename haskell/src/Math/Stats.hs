module Math.Stats where
import Math.Constants

erfc :: Double -> Double
erfc x
    | x < 0     = 2.0 - erfc (-x)
    | otherwise =
        let p  = 0.3275911
            a1 = 0.254829592
            a2 = -0.284496736
            a3 = 1.421413741
            a4 = -1.453152027
            a5 = 1.061405429
            t  = 1.0 / (1.0 + p * x)
            -- Horner's method for polynomial evaluation
            poly = t * (a1 + t * (a2 + t * (a3 + t * (a4 + t * a5))))
        in poly * exp (-x * x)

heaviside :: Double -> Int
heaviside x
    | x < 0 = 0
    | otherwise = 1

sigmoid :: Double -> Double
sigmoid x = x / (1 + exp (-x))

stdNormPdf0 :: Double -> Double
stdNormPdf0 z = invSqrt2Pi * exp ((-0.5) * z ** 2)

stdNormCdf0 :: Double -> Double
stdNormCdf0 z = 0.5 * erfc (-z / sqrt2)

normCdf1 :: Double -> Double
normCdf1 x
    | x >= 0.0 = 1.0 - z * y
    | otherwise =  z*y
    where
        u = 1.0 / (1.0 + 0.2316419 * abs x)
        y = u * (0.319381530 + u * (-0.356563782 + u * (1.781477937 + u * (-1.821255978 + u * 1.330274429))))
        z = exp (-0.5 * x * x) / sqrt (2 * pi)

normCdf2 :: Double -> Double
normCdf2 x
    | x >= 0.0 = 0.5 * (1.0 + sqrt (1.0 - exp ((-a) * x * x)))
    | otherwise = 0.5 * (1.0 - sqrt (1.0 - exp ((-a) * x * x)))
    where a = 0.647 - 0.021 * abs x

normCdf3 :: Double -> Double
normCdf3 x = 0.5 * (1.0 + tanh(19.5 * x - 55.5 * atan (35.0 * x / 111.0)))
    where x = x / sqrt (2 * pi)

normCdf3a :: Double -> Double
normCdf3a x = 0.5 * (1.0 + tanh (7.7784 * x - 55.49 * atan (0.1258 * x)))

normCdf4 :: Double -> Double
normCdf4 x
    | x >= 0.0 = 1.0 - 0.5/y
    | otherwise = 0.5 / y
    where u = abs x
          y = (1 + u*(0.196854 + u*(0.115194 + u*(0.000344 + u*0.019527)))) ^ (4::Int)
