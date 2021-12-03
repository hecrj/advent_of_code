module Main where

import Data.List.Split
import Data.Maybe


data Direction
    = Forward Int
    | Down Int
    | Up Int


data Position
    = Position { x :: Int, depth :: Int }


main :: IO ()
main = do
    directions <- fmap (catMaybes . map parseDirection . lines) getContents

    let
        Position x depth =
            foldl move (Position 0 0) directions

    putStrLn (show (x * depth))


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
move (Position x depth) (Forward n) =
    Position (x + n) depth
move (Position x depth) (Down n) =
    Position x (depth + n)
move (Position x depth) (Up n) =
    Position x (depth - n)
