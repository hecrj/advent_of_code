module AdventOfCode.Y2021.DiracDice
    ( day
    ) where

import qualified AdventOfCode
import qualified AdventOfCode.Tuple as Tuple
import qualified Data.List as List
import qualified Data.List.Split as List


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Dirac Dice" part1 part2
    where
        part1 =
            losingScore . head . reverse . play deterministic100Die . parse

        part2 =
            const 0


parse :: String -> Game
parse =
    Game 0 . fmap parsePlayer . lines


data Player
    = Player
        { _position :: Int
        , _score :: Int
        }


parsePlayer :: String -> Player
parsePlayer input =
    case List.splitOn ": " input of
        [ _, position ] ->
            Player (read position) 0

        _ ->
            error "invalid player!"


newtype Die
    = Die (Int -> Int)


deterministic100Die :: Die
deterministic100Die =
    Die ((+) 1 . flip (mod) 100)


data Game
    = Game
        { _turn :: Int
        , _players :: [Player]
        }


play :: Die -> Game -> [Game]
play die =
    List.unfoldr (fmap Tuple.dupe . next die)


next :: Die -> Game -> Maybe Game
next _ (Game _ []) =
    Nothing
next (Die die) (Game turn ((Player position score) : rest))
    | all (flip (<) 1000) (score : fmap _score rest) =
        let
            roll =
                turn * 3

            result =
                sum $ fmap die [roll..roll + 2]

            newPosition =
                (position + result - 1) `mod` 10 + 1

            newScore =
                score + newPosition
        in
        Just $ Game (turn + 1) (rest ++ [ Player newPosition newScore ])
    | otherwise =
        Nothing


losingScore :: Game -> Int
losingScore (Game _ []) =
    0
losingScore (Game turn (loser : _)) =
    turn * 3 * _score loser
