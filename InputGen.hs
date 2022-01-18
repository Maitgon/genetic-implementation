module InputGen where

import System.Random
import System.Random.Stateful (newStdGen)

nCandidates :: Int
nCandidates = 12

totalLaws :: Int
totalLaws = 7

nextChar :: Char -> Char
nextChar a = toEnum (fromEnum a + 1)

formPartySeats :: Int -> StdGen -> Char -> [String]
formPartySeats total gen curr
    | total == 0 = []
    | otherwise  = ([curr] ++ " : " ++ show points) : formPartySeats (total-1) new (nextChar curr)
        where (points, new) = uniformR (20 :: Int, 30 :: Int) gen

formLaw :: Int -> Int -> StdGen -> (String, StdGen)
formLaw n total gen = (firstPart ++ secondPart, snd $ split gen)
    where firstPart  = "Law " ++ show n ++ ":\n"
          secondPart = unlines $ formLawAux total gen 'A'

formLawAux :: Int -> StdGen -> Char -> [String]
formLawAux total gen curr
    | total == 0 = []
    | otherwise  = ([curr] ++ " : " ++ show points) : formLawAux (total-1) new (nextChar curr)
        where (points, new) = uniformR (-10 :: Int, 10 :: Int) gen

formLawAll :: Int -> StdGen -> String
formLawAll n gen
    | n == totalLaws = currState
    | otherwise        = currState ++ "\n" ++ formLawAll (n+1) new
        where (currState, new) = formLaw n nCandidates gen


main :: IO ()
main = do
    let firstPart = "Input5.\n\nNumber of Laws:\n" ++ show nCandidates ++ "\n\n"

    gen <- newStdGen

    let secondPart = "Party Seats:\n" ++ unlines (formPartySeats nCandidates gen 'A')

    let thirdPart = "\nLeyes:\n\n" ++ formLawAll 1 gen

    let sol = firstPart ++ secondPart ++ thirdPart

    writeFile "inputs/randomInputs/input5.txt" sol

