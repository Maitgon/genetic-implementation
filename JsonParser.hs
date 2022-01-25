{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module JsonParser where

import GHC.Generics ( Generic )
import Data.Aeson ( decode, (.:), withObject, FromJSON(parseJSON) )
import qualified Data.ByteString.Lazy as B
import Data.Maybe ( fromJust )
import qualified Data.Map as M

import System.Random ( uniformR, RandomGen(split), StdGen )
import System.Random.Stateful (newStdGen)

data Datos = Datos {
      informacion :: Informacion
    , totales  :: Totales
    , votaciones :: [Votaciones]
} deriving (Generic, Show)

instance FromJSON Datos where
    parseJSON = withObject "Law" $ \v -> Datos
        <$> v .: "informacion"
        <*> v .: "totales"
        <*> v .: "votaciones"

data Informacion = Informacion {
      sesion :: Int
    , numeroVotacion :: Int
    , fecha :: String
    , titulo :: String
    , textoExpediente :: String
    , tituloSubGrupo :: String
    , textoSubGrupo :: String
    , votacionesConjuntas :: [Int]
} deriving (Generic, Show)

instance FromJSON Informacion where
    parseJSON = withObject "informacion" $ \v -> Informacion
        <$> v .: "sesion"
        <*> v .: "numeroVotacion"
        <*> v .: "fecha"
        <*> v .: "titulo"
        <*> v .: "textoExpediente"
        <*> v .: "tituloSubGrupo"
        <*> v .: "textoSubGrupo"
        <*> v .: "votacionesConjuntas"

data Totales = Totales {
      asentimiento :: String
    , presentes :: Int
    , afavor :: Int
    , enContra :: Int
    , abstenciones :: Int
    , noVotan :: Int
} deriving (Generic, Show)

instance FromJSON Totales where
    parseJSON = withObject "totales" $ \v -> Totales
        <$> v .: "asentimiento"
        <*> v .: "presentes"
        <*> v .: "afavor"
        <*> v .: "enContra"
        <*> v .: "abstenciones"
        <*> v .: "noVotan"

data Votaciones = Votaciones {
      asiento :: String
    , diputado :: String
    , grupo :: String
    , voto :: String
} deriving (Generic, Show)

instance FromJSON Votaciones where
    parseJSON = withObject "votaciones" $ \v -> Votaciones
        <$> v .: "asiento"
        <*> v .: "diputado"
        <*> v .: "grupo"
        <*> v .: "voto"

type Partido = String

data Voto = Favor | Abstiene | Contra
    deriving (Eq, Show)


getVotings :: B.ByteString -> M.Map Partido Voto
getVotings byteString = M.delete "GMx" $ M.delete "GPlu" todos
    where parsed = decode byteString
          votos  = votaciones $ fromJust parsed
          todos  = voting $ zip (map grupo votos) (map voto votos)

voting :: [(Partido, String)] -> M.Map Partido Voto
voting votacion = M.map decide $ M.fromListWith (+) $ map aux votacion
    where aux :: (Partido, String) -> (Partido, Int)
          aux (x, "S\237") = (x, 1)
          aux (x, "No") = (x, -1)
          aux (x, _) = (x, 0)
          decide x
            | x < -3 = Contra
            | x > 3 = Favor
            | otherwise = Abstiene

data Law = Law {
      numero :: String
    , nombre :: String
    , votos  :: M.Map Partido Voto
} deriving (Eq, Show)


more :: [Int] -> IO [Law]
more laws = do
    let byteStrings = [B.readFile $ "../../LAW_PROBLEM/Genetic_Implementation/LAW_input/data/VOT" ++ show x ++ ".json" | x <- laws]
    votos <- mapM (fmap getVotings) byteStrings
    leyes <- mapM (fmap $ textoExpediente . informacion . fromJust . decode) byteStrings
    -- same as sequence $ (map . fmap) getVotings byteStrings
    let newNames = [show x | x <- [1..length laws]]
    let sol = zipWith3 Law newNames leyes votos
    return sol

---------------------
-- Escribir en txt --
---------------------

parsePC :: [Int] -> String
parsePC [] = []
parsePC [x] = show x
parsePC (x:xs) = show x ++ "," ++ parsePC xs

start :: [Int] -> String
start xs = "Input " ++ parsePC xs ++ ".\n\
            \\n\
            \Number of laws:\n\
            \" ++ show (length xs) ++ "\n\
            \\n\
            \Party Seats:\n\
            \GCUP-EC-GC : 34\n\
            \GCs : 9\n\
            \GEH Bildu : 5\n\
            \GP : 88\n\
            \GR : 13\n\
            \GS : 120\n\
            \GV : 6\n\
            \GVOX : 52\n\
            \\n\
            \Leyes:\n\
            \\n"

parseLaw :: Law -> StdGen -> (String, StdGen)
parseLaw law gen = (firstPart ++ secondPart, newGen)
    where firstPart = "Law " ++ numero law ++ "\n"
          newParse = parseLawAux (M.assocs $ votos law) gen
          secondPart = unlines $ map fst newParse
          newGen = snd $ split (snd $ last newParse)

parseLawAux :: [(Partido, Voto)] -> StdGen -> [(String, StdGen)]
parseLawAux [] _ = []
parseLawAux (v:votes) gen
    | snd v == Favor = (fst v ++ " : " ++ show newNum, newGen) : parseLawAux votes newGen
    | snd v == Abstiene = (fst v ++ " : 0", newGen) : parseLawAux votes newGen
    | snd v == Contra = (fst v ++ " : " ++ show (-newNum), newGen) : parseLawAux votes newGen
        where (newNum, newGen) = uniformR (3 :: Int, 10 :: Int) gen


parseLawAll :: [Law] -> StdGen -> String
parseLawAll [] gen = []
parseLawAll (l:laws) gen = currState ++ "\n" ++ parseLawAll laws new
        where (currState, new) = parseLaw l gen


main :: IO ()
main = do
    -- Max law number is 27
    let laws1 = [1..27]
    sol <- more laws1
    --print sol
    --print (numero $ sol !! 2)

    gen <- newStdGen
    let allLaws = parseLawAll sol gen
    --putStrLn allLaws

    let final = init $ start laws1 ++ allLaws
    --putStr final

    writeFile "inputs/inputsBuenas/input6.txt" final

