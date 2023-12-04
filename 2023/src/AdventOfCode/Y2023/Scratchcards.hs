module AdventOfCode.Y2023.Scratchcards (day) where

import qualified AdventOfCode
import qualified Data.List as List
import qualified Data.List.Split as List


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Scratchcards" part1 part2


part1 :: AdventOfCode.Solution
part1 =
    sum . fmap (score . parse) . lines


part2 :: AdventOfCode.Solution
part2 =
    const 0


data Card = Card
    { _targets :: [Int]
    , _numbers :: [Int]
    }


parse :: String -> Card
parse card =
    let
        [_label, contents] = List.splitOn ": " card
        [targets, numbers] = List.splitOn " | " contents
    in
        Card
            (read <$> (words targets))
            (read <$> (words numbers))


score :: Card -> Int
score (Card targets numbers) =
    let
        winning = List.intersect numbers targets
    in
        if null winning
            then 0
            else 2 ^ (List.length winning - 1)