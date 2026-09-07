module Main where
import Math.Constants (invSqrt2Pi)

main :: IO ()
main = putStrLn ("Hello! The inverse square root of 2Pi is: " ++ show invSqrt2Pi)