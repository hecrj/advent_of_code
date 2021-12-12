module AdventOfCode.Y2021.TheTreacheryOfWhales
    ( day
    ) where

import qualified AdventOfCode
import qualified Data.List.Split as List


day :: AdventOfCode.Day
day =
    AdventOfCode.day "The Treachery of Whales" part1 part2
    where
        part1 =
            minimumX fuel . parse

        part2 =
            minimumX fuelTriangular . parse

        parse =
            fmap parseCrab . List.splitOn ","

        minimumX f crabs =
            let
                minX =
                    minimum (fmap x crabs)

                maxX =
                    maximum (fmap x crabs)
            in
            minimum (fmap ((flip f) crabs) [minX..maxX])


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
    sum . fmap (triangular . abs . (-) n . x)
    where
        triangular n =
            (n * (n + 1)) `div` 2


fuelTriangular :: Int -> [Crab] -> Int
fuelTriangular n =
    sum . fmap (triangular . abs . (-) n . x)
    where
        triangular n =
            (n * (n + 1)) `div` 2
