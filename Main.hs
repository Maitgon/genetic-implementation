module Main where

import GeneticPact (geneticPact, totalPoints, mutFactor)
import qualified Data.Map as M
import Control.Monad

main :: IO ()
main = do


    input1 <- readFile "inputs/randomInputs/input1.txt"
    input2 <- readFile "inputs/randomInputs/input2.txt"
    input3 <- readFile "inputs/randomInputs/input3.txt"
    input4 <- readFile "inputs/randomInputs/input4.txt"
    input5 <- readFile "inputs/randomInputs/input5.txt"

    putStrLn $ show mutFactor ++ "% mutation"

    -- Input 1:
    -- Candidates: 7
    -- Total Laws: 20

    sols1 <- replicateM 50 $ do
        geneticPact 200 input1
    
    putStrLn "\n\n--------------"
    putStrLn "-- Input 1. --"
    putStrLn "--------------"

    let max1 = maximum sols1
    let totalPoints1 = totalPoints input1
    let sols1' = filter (> -totalPoints1) sols1

    putStrLn $ "\ntotalSeats: " ++ show totalPoints1

    putStrLn $ "\nMin: " ++ show (minimum sols1')
    putStrLn $ "Max: " ++ show max1
    putStrLn $ "Invalid: " ++ show (length sols1 - length sols1')
    putStrLn $ "Mean: " ++ show (mean sols1')
    putStrLn $ "Variance: " ++ show (variance sols1')


    -- Input 2:
    -- Candidates: 5
    -- Total Laws: 12

    sols2 <- replicateM 50 $ do
        geneticPact 200 input2
    
    putStrLn "\n\n--------------"
    putStrLn "-- Input 2. --"
    putStrLn "--------------"

    let max2 = maximum sols2
    let totalPoints2 = totalPoints input2
    let sols2' = filter (> -totalPoints2) sols2

    putStrLn $ "\ntotalSeats: " ++ show totalPoints2

    putStrLn $ "\nMin: " ++ show (minimum sols2')
    putStrLn $ "Max: " ++ show max2
    putStrLn $ "Invalid: " ++ show (length sols2 - length sols2')
    putStrLn $ "Mean: " ++ show (mean sols2')
    putStrLn $ "Variance: " ++ show (variance sols2')


    -- Input 3:
    -- Candidates: 10
    -- Total Laws: 15

    sols3 <- replicateM 50 $ do
        geneticPact 200 input3
    
    putStrLn "\n\n--------------"
    putStrLn "-- Input 3. --"
    putStrLn "--------------"

    let max3 = maximum sols3
    let totalPoints3 = totalPoints input3
    let sols3' = filter (> -totalPoints3) sols3

    putStrLn $ "\ntotalSeats: " ++ show totalPoints3

    putStrLn $ "\nMin: " ++ show (minimum sols3')
    putStrLn $ "Max: " ++ show max3
    putStrLn $ "Invalid: " ++ show (length sols3 - length sols3')
    putStrLn $ "Mean: " ++ show (mean sols3')
    putStrLn $ "Variance: " ++ show (variance sols3')


    -- Input 4:
    -- Candidates: 6
    -- Total Laws: 12

    sols4 <- replicateM 50 $ do
        geneticPact 200 input4
    
    putStrLn "\n\n--------------"
    putStrLn "-- Input 4. --"
    putStrLn "--------------"

    let max4 = maximum sols4
    let totalPoints4 = totalPoints input4
    let sols4' = filter (> -totalPoints4) sols4

    putStrLn $ "\ntotalSeats: " ++ show totalPoints4

    putStrLn $ "\nMin: " ++ show (minimum sols4')
    putStrLn $ "Max: " ++ show max4
    putStrLn $ "Invalid: " ++ show (length sols4 - length sols4')
    putStrLn $ "Mean: " ++ show (mean sols4')
    putStrLn $ "Variance: " ++ show (variance sols4')


    -- Input 5:
    -- Candidates: 12
    -- Total Laws: 7

    sols5 <- replicateM 50 $ do
        geneticPact 200 input5
    
    putStrLn "\n\n--------------"
    putStrLn "-- Input 5. --"
    putStrLn "--------------"

    let max5 = maximum sols5
    let totalPoints5 = totalPoints input5
    let sols5' = filter (> -totalPoints5) sols5

    putStrLn $ "\ntotalSeats: " ++ show totalPoints5

    putStrLn $ "\nMin: " ++ show (minimum sols5')
    putStrLn $ "Max: " ++ show max5
    putStrLn $ "Invalid: " ++ show (length sols5 - length sols5')
    putStrLn $ "Mean: " ++ show (mean sols5')
    putStrLn $ "Variance: " ++ show (variance sols5')






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