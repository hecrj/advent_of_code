module Main where

import qualified Data.List.Split as List


main :: IO ()
main = do
    crabs <- fmap (fmap parseCrab . List.splitOn ",") getContents

    let
        minX =
            minimum (fmap x crabs)

        maxX =
            maximum (fmap x crabs)

        minFuel =
            minimum (fmap ((flip fuel) crabs) [minX..maxX])

    putStrLn $ show minFuel


data Crab
    = Crab Int
    deriving Show


parseCrab :: String -> Crab
parseCrab =
    Crab . read


x :: Crab -> Int
x (Crab p) =
    p


fuel :: Int -> [Crab] -> Int
fuel n =
    sum . fmap (abs . (-) n . x)
