module AdventOfCode.Y2021.Dive
    ( day
    ) where

import Data.List.Split
import Data.Maybe
import qualified AdventOfCode


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Dive" part1 part2
    where
        part1 =
            result move . parse

        part2 =
            result move2 . parse

        parse =
            catMaybes . map parseDirection . lines

        result f directions =
            let
                Position x depth _ =
                    foldl f (Position 0 0 0) directions
            in
            x * depth


data Direction
    = Forward Int
    | Down Int
    | Up Int


data Position
    = Position { _x :: Int, _depth :: Int, _aim :: Int }


parseDirection :: String -> Maybe Direction
parseDirection s =
    case splitOn " " s of
        [ "forward", n ] ->
            Just $ Forward (read n)

        [ "down", n ] ->
            Just $ Down (read n)

        [ "up", n ] ->
            Just $ Up (read n)

        _ ->
            Nothing


move :: Position -> Direction -> Position
move (Position x depth aim) (Forward n) =
    Position (x + n) depth aim
move (Position x depth aim) (Down n) =
    Position x (depth + n) aim
move (Position x depth aim) (Up n) =
    Position x (depth - n) aim


move2 :: Position -> Direction -> Position
move2 (Position x depth aim) (Forward n) =
    Position (x + n) (depth + (aim * n)) aim
move2 (Position x depth aim) (Down n) =
    Position x depth (aim + n)
move2 (Position x depth aim) (Up n) =
    Position x depth (aim - n)
