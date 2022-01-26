module Main where

import GeneticPact (geneticPact, totalPoints)
import qualified Data.Map as M
import Control.Monad

main :: IO ()
main = do


    input1 <- readFile "inputs/inputsBuenas/input1.txt"
    input2 <- readFile "inputs/randomInputs/input2.txt"
    input3 <- readFile "inputs/randomInputs/input3.txt"
    input4 <- readFile "inputs/randomInputs/input4.txt"
    input5 <- readFile "inputs/randomInputs/input5.txt"

    let inputsNums = [1,2,3,4,5,6]
    inputs <- sequence [readFile $ "inputs/inputsBuenas/input" ++ show x ++ ".txt" | x <- inputsNums]

    let mutFactor = [0.01, 0.02, 0.05, 0.1, 0.15]
    let iters = 200

    printAll [printResults inp 200 mut | inp <- inputs, mut <- mutFactor]


printResults :: String -> Int -> Float -> IO ()
printResults input iters mutFactor = do
    sols <- replicateM 50 $ do
        geneticPact 200 input mutFactor
    
    putStrLn "\n\n--------------"
    putStrLn $ "-- " ++ head (lines input) ++ ".--"
    putStrLn "--------------"

    let max1 = maximum sols
    let totalPoints1 = totalPoints input
    let sols1' = filter (> -totalPoints1) sols

    putStrLn $ "\ntotalSeats: " ++ show totalPoints1
    putStrLn $ "MutFactor: " ++ show mutFactor

    putStrLn $ "\nMin: " ++ (if sols1' == [] then "no valid solutions" else show (minimum sols1'))
    putStrLn $ "Max: " ++ show max1
    putStrLn $ "Invalid: " ++ show (length sols - length sols1')
    putStrLn $ "Mean: " ++ show (mean sols1')
    putStrLn $ "Variance: " ++ show (variance sols1')

printAll :: [IO ()] -> IO ()
printAll [] = putStrLn "final"
printAll (x:xs) = do
    x
    printAll xs



frequency :: [Int] -> M.Map Int Int
frequency xs = M.fromListWith (+) [(x, 1) | x <- xs]

prettyPrint :: M.Map Int Int -> String
prettyPrint m = M.foldlWithKey f "Appearances:\n" m
    where f result k a = result ++ "" ++ show k ++ " -> " ++ show a ++ "\n"

mean :: [Int] -> Float
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

variance :: [Int] -> Float
variance xs = sum (zipWith (*) aux2 aux2) / fromIntegral (length xs)
    where meant = mean xs
          aux1 = map fromIntegral xs
          aux2 = [x - meant | x <- aux1]