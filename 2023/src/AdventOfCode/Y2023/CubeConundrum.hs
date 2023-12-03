module AdventOfCode.Y2023.CubeConundrum (day) where

import qualified AdventOfCode
import qualified Data.List.Split as List

import qualified Data.List as List
import Prelude hiding (id)


day :: AdventOfCode.Day
day = AdventOfCode.day "Cube Conundrum" part1 part2


part1 :: AdventOfCode.Solution
part1 =
    sum . fmap id . filter isPossible . fmap parse . lines


part2 :: AdventOfCode.Solution
part2 =
    sum . fmap (power . smallestBag) . fmap parse . lines


data Game = Game
    { id :: Int
    , _rounds :: [Round]
    }


parse :: String -> Game
parse game =
    let
        [label, rounds] = List.splitOn ": " game
        id = read $ last $ List.splitOn " " label
    in
        Game id (fmap parseRound (List.splitOn "; " rounds))


isPossible :: Game -> Bool
isPossible (Game _ rounds) =
    all isRoundPossible rounds


smallestBag :: Game -> Bag
smallestBag (Game _ rounds) =
    Bag (max red) (max green) (max blue)
  where
    max f = maximum $ fmap f rounds


data Round = Round
    { red :: Int
    , green :: Int
    , blue :: Int
    }


parseRound :: String -> Round
parseRound round =
    Round (color "red") (color "green") (color "blue")
  where
    cubes = fmap (List.splitOn " ") (List.splitOn ", " round)
    color label =
        maybe 0 (read . head) $ List.find (\[_cubes, color] -> color == label) cubes


isRoundPossible :: Round -> Bool
isRoundPossible (Round red green blue) =
    red <= 12 && green <= 13 && blue <= 14


data Bag = Bag
    { _red :: Int
    , _green :: Int
    , _blue :: Int
    }


power :: Bag -> Int
power (Bag red green blue) =
    red * green * blue
