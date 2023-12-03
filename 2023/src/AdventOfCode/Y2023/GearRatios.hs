module AdventOfCode.Y2023.GearRatios (day) where

import qualified AdventOfCode
import qualified Data.Char as Char
import qualified Data.List as List

import Data.Maybe (fromMaybe, mapMaybe)
import Relude.List ((!!?))


day :: AdventOfCode.Day
day = AdventOfCode.day "Gear Ratios" part1 part2


part1 :: AdventOfCode.Solution
part1 =
    sum . partNumbers . parse


part2 :: AdventOfCode.Solution
part2 =
    const 0


newtype Schematic = Schematic [[Char]]


data Number = Number
    { value :: Int
    , _cells :: [Cell]
    }
    deriving (Show)


data Cell = Cell
    { _i :: Int
    , _j :: Int
    }
    deriving (Show)


parse :: String -> Schematic
parse =
    Schematic . lines


partNumbers :: Schematic -> [Int]
partNumbers schematic =
    fmap value $ filter (isPartNumber schematic) $ numbers schematic


numbers :: Schematic -> [Number]
numbers (Schematic rows) =
    concatMap (uncurry scanRow) (enumerate rows)
  where
    scanRow :: Int -> String -> [Number]
    scanRow i row =
        let
            groups =
                List.groupBy
                    (\(_, a) (_, b) -> Char.isDigit a == Char.isDigit b)
                    (enumerate row)
        in
            mapMaybe (toNumber i) groups

    toNumber :: Int -> [(Int, Char)] -> Maybe Number
    toNumber i cells =
        let
            digits = filter Char.isDigit $ fmap snd cells
        in
            if null digits
                then Nothing
                else Just $ Number (read digits) (Cell i . fst <$> cells)


isPartNumber :: Schematic -> Number -> Bool
isPartNumber schematic (Number _ cells) =
    any (any (isSymbol . at schematic) . neighbors) cells


at :: Schematic -> Cell -> Char
at (Schematic rows) (Cell i j) =
    fromMaybe '.' (rows !!? i >>= (!!? j))


isSymbol :: Char -> Bool
isSymbol '.' = False
isSymbol c = not $ Char.isDigit c


neighbors :: Cell -> [Cell]
neighbors (Cell i j) =
    uncurry Cell
        <$> [ (ni, nj)
            | ni <- [i - 1 .. i + 1]
            , nj <- [j - 1 .. j + 1]
            , ni /= i || nj /= j
            ]


enumerate :: [a] -> [(Int, a)]
enumerate =
    zip [0 ..]
