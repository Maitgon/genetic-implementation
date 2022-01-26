module GeneticPact where

import Data.List.Split
import qualified Data.Map as M
import Data.List
import System.Random
import Control.Monad


type Voto = Int

type Solucion = [[Voto]]
type Puntos = Int

type Seats = Int

signo :: Int -> Voto
signo x
    | x > 0     = 1
    | x < 0     = -1
    | otherwise = 0

solInicial :: [[Puntos]] -> Solucion
solInicial = map (map signo)


lawsApproved :: [[Int]] -> [Int] -> [Int]
lawsApproved xxs xs = map (\x -> if x > 0 then 1 else -1) $ multRow xxs xs

pointsTotal :: [[Int]] -> [Int] -> [Int]
pointsTotal points laws = map sum . transpose $ newPoints
    where newPoints = zipWith (\x y -> map (*x) y) laws points

points :: [[Int]] -> [[Int]] -> [Int] -> [Int]
points votes point seats = pointsTotal point approved
    where approved = lawsApproved votes seats

points' point seats votes = points votes point seats

originalPoints :: [[Puntos]] -> [Seats] -> [Puntos]
originalPoints point seats = points solInit point seats
    where solInit = solInicial point

whoInPact :: Solucion -> [[Puntos]] -> [Int]
whoInPact new point = whoInPactAux (transpose new) (transpose old) 0
    where old = solInicial point

whoInPactAux :: [[Int]] -> [[Int]] -> Int -> [Int]
whoInPactAux [] _ _ = []
whoInPactAux _ [] _ = []
whoInPactAux (n:new) (o:old) pos = if n == o
                                   then whoInPactAux new old (pos+1)
                                   else pos : whoInPactAux new old (pos+1)

isValidPact :: [[Puntos]] -> [Seats] -> Solucion -> Bool
isValidPact point seats sol = and [puntosInit !! i < puntosNew !! i | i <- pact] && pact /= []
    where solInit = solInicial point
          puntosInit = points solInit point seats
          puntosNew = points sol point seats
          pact = whoInPact sol point

fitnessFunc :: [[Puntos]] -> [Seats] -> Solucion -> Int
fitnessFunc point seats sol = if valid
                              then pointsWin
                              else pointsWin - maxPoints*2
    where pointsWin = head $ points sol point seats
          maxPoints = sum $ map abs $ head $ transpose point
          valid = isValidPact point seats sol

totalPoints :: String -> Int
totalPoints input = sum $ map abs $ head $ transpose puntosM
    where _:_:_:_:xs = splitOn "\n\n" input
          laws = map getLaw xs
          puntosM = map M.elems laws


          

-------------------------
-- Auxiliar functions. --
-------------------------

-- Convert a list of elements in a list of unitary lists
-- of the same elements
elevate :: [a] -> [[a]]
elevate = map (: [])

-- NxM matrix multiplication by a N-vector row.
multRow :: [[Int]] -> [Int] -> [Int]
multRow xxs xs = map sum seatsBy
    where seatsBy = map (zipWith (*) xs) xxs

randomList :: StdGen -> Int -> Int -> [Int]
randomList gen bot top = number : randomList new bot top
    where (number, new) = uniformR (bot, top) gen

initialization :: Int -> Int -> IO [[Int]]
initialization nLaws mParties = do
    gen <- newStdGen
    let loop 0 _ _ = []
        loop n m gen = take mParties (randomList new1 (-1) 1) : loop (n-1) m new2
            where (new1, new2) = System.Random.split gen
    return (loop nLaws mParties gen)

changeList :: Int -> a -> [a] -> [a]
changeList 0 item [] = [item]
changeList _ item [] = undefined
changeList 0 item (x:xs) = item:xs
changeList n item (x:xs) = x : changeList (n-1) item xs

-- A initialization that selects our party and other party at random
-- and selects two random laws to change their vote on
initialization2 :: Int -> Int -> Solucion -> IO [[Int]]
initialization2 nLaws mParties init = do

    -- We transpose it to a list of party votes
    let tsolInit = transpose $ solInicial init

    -- We select at random the laws and party to change
    gen <- newStdGen
    let primero = 0
    let (segundo, new) = randomR (1, mParties-1) gen
    let (firstL, new') = randomR (0, nLaws-1) new
    let (secondL, _) = randomR (0, nLaws-1) new'

    -- Create the pacts
    let firstList = tsolInit !! primero
    let newFirst = changeList firstL (firstList !! firstL * (-1)) firstList
    let secondList = tsolInit !! segundo
    let newSecond = changeList secondL (secondList !! secondL * (-1)) secondList

    -- Put the pacts
    let newSol = changeList primero newFirst tsolInit
    return $ transpose $ changeList segundo newSecond newSol

-- Get gaps between probabilites in a list:
-- 5 element list : [1.0, 1.8, 2.4, 2.8, 3.0]
-- Used for selecting the best parents with higher probability.
probabilitiesDown :: Int -> Int -> Float -> [Float]
probabilitiesDown 0 _ _ = []
probabilitiesDown m n f = newF : probabilitiesDown (m-1) n newF
    where newF = f + (fromIntegral m / fromIntegral n)

-- Select one of the parents at random
-- but need to be fed a weighted list
-- modified as the list that probabilitiesDown gives.
randomParents :: [Float] -> Float -> Int -> Int
randomParents [] _ pos = pos
randomParents (x:prob) f pos
    | x < f = randomParents prob f (pos+1)
    | otherwise = pos

-- Function to make a child solution
-- given two parents solution
makeChild :: Solucion -> Solucion -> StdGen -> Solucion
makeChild [] _ _ = []
makeChild _ [] _ = []
makeChild (x:xs) (y:ys) gen = if ran == 1
                              then x : makeChild xs ys new
                              else y : makeChild xs ys new
    where (ran, new) = randomR (1 :: Int, 2 :: Int) gen

mutate :: Solucion -> Float -> IO Solucion
mutate sol mutFactor = do
    let tSol = transpose sol
    gen <- newStdGen
    return $ transpose $ mutateAux 0 (length sol) (length $ head sol) tSol gen mutFactor

mutateAux :: Int -> Int -> Int -> Solucion -> StdGen -> Float -> Solucion
mutateAux count nLaws mParties sol gen mutFactor
    | numb < mutFactor && count /= mParties = mutateAux (count+1) nLaws mParties
                                                        (changeList segundo newSecond newSol) 
                                                        new'' mutFactor
    | count /= mParties = mutateAux (count+1) nLaws mParties sol gen' mutFactor
    | otherwise = sol
    where (numb, gen') = random gen

    -- We select at random the laws and party to change
          primero = count
          segundo = head $ filter (/= count) $ randomRs (0, mParties-1) gen'
          (new, _) = System.Random.split gen'
          (firstL, new') = randomR (0, nLaws-1) new
          (secondL, new'') = randomR (0, nLaws-1) new'

    -- Create the pacts
          firstList = sol !! primero
          newFirst = changeList firstL (firstList !! firstL * (-1)) firstList
          secondList = sol !! segundo
          newSecond = changeList secondL (secondList !! secondL * (-1)) secondList

    -- Put the pacts
          newSol = changeList primero newFirst sol

          




---------------------
-- Parsing string. --
---------------------

getNumberLaws :: String -> Int
getNumberLaws s = read $ last $ lines s

getPartySeats :: String -> M.Map String Int
getPartySeats s = M.fromList step3
    where valid = tail $ lines s
          step2 = map (splitOn " : ") valid
          step3 = map (\[x,y] -> (x, read y :: Int)) step2

getLaw :: String -> M.Map String Int
getLaw = getPartySeats


geneticPact :: Int -> String -> Float -> IO Int
geneticPact iter input mutFactor = do
    --input <- readFile "inputs/input1.txt"
    --putStrLn input
    let _:i1:i2:_:xs = splitOn "\n\n" input
    --let nLaws = getNumberLaws i1
    --print nLaws
    let partySeats = getPartySeats i2
    --print partySeats
    let laws = map getLaw xs
    --print laws
    let puntosM = map M.elems laws
    --print puntosM
    let votoInicial = solInicial puntosM
    --print $ solInicial puntosM
    let seats = M.elems partySeats
    --let algo = multRow votoInicial seats
    --print algo
    --let approved = lawsApproved votoInicial seats
    --print approved
    --let orig = originalPoints puntosM seats
    --print orig
    randomInit <- initialization2 (length laws) (length seats) puntosM
    --print randomInit
    --let newPoints = points randomInit puntosM seats
    --print newPoints
    --print $ whoInPact randomInit puntosM

    let popu = 500

    randomInit2 <- replicateM popu $ initialization2 (length laws) (length seats) puntosM
    --print randomInit2
    --print $ map (flip whoInPact puntosM) randomInit2
    --print $ map (isValidPact puntosM seats) randomInit2
    --print $ map (points' puntosM seats) randomInit2
    --print $ points votoInicial puntosM seats
    --print $ zip3 --(map (flip whoInPact puntosM) randomInit2)
                 --(map (isValidPact puntosM seats) randomInit2)
                 --(map (points' puntosM seats) randomInit2)
                 --(map (fitnessFunc puntosM seats) randomInit2)
    
    let sorted1 = take 50 $ reverse $ sortOn (fitnessFunc puntosM seats) randomInit2
    -- print $ fitnessFunc puntosM seats (head sorted1)

    -- Looping
    let loop 0 sol = (pure . reverse) $ sortOn (fitnessFunc puntosM seats) sol
        loop n sol = do
            let ordSol = reverse $ sortOn (fitnessFunc puntosM seats) sol
            let prob = probabilitiesDown popu popu 0.0
            newPopu <- replicateM (popu-1) $ do
                gen <- newStdGen
                let [next1, next2] = take 2 $ randomRs (0.0 , last prob) gen
                let parent1 = randomParents prob next1 0
                let parent2' = randomParents prob next2 0
                let parent2 = if parent2' == parent1 then parent2' `div` 2 else parent2'
                let child = transpose $ makeChild (transpose (ordSol !! parent1))
                                                  (transpose (ordSol !! parent2)) gen

                -- Mutation here

                mutate child mutFactor

                -- return child

            -- We return the best child too
            loop (n-1) (head ordSol : newPopu)
    
    best <- head <$> loop iter randomInit2
    --print best
    --print $ points votoInicial puntosM seats
    --print $ points best puntosM seats
    --print $ isValidPact puntosM seats best
    --print $ fitnessFunc puntosM seats best
    return $ fitnessFunc puntosM seats best

--main :: IO ()
--main = do
--    input <- readFile "inputs/randomInputs/input1.txt"
--    sol <- geneticPact 20 input
--    print sol