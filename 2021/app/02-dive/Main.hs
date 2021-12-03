module Main where

import Data.List.Split
import Data.Maybe


data Direction
    = Forward Int
    | Down Int
    | Up Int


data Position
    = Position { x :: Int, depth :: Int, aim :: Int }


main :: IO ()
main = do
    directions <- fmap (catMaybes . map parseDirection . lines) getContents
    putStrLn $ show (result move directions)
    putStrLn $ show (result move2 directions)
    where
        result f directions =
            let
                Position x depth _ =
                    foldl f (Position 0 0 0) directions
            in
            x * depth


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
