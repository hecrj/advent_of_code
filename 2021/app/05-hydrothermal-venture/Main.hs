module Main where

import Data.List as List
import Data.List.Split
import Data.Map.Strict as Map
import Data.Maybe


main :: IO ()
main = do
    lines <- fmap (catMaybes . fmap parseLine . lines) getContents

    let
        grid =
            List.foldr drawLine emptyGrid lines

    putStrLn $ show (overlappingPoints grid)


data Line
    = Horizontal { start :: Point, length :: Int }
    | Vertical { start :: Point, length :: Int }

    deriving Show


parseLine :: String -> Maybe Line
parseLine input =
    case splitOn " " input of
        [ start, _, end ] ->
            case ( fmap read $ splitOn "," start, fmap read $ splitOn "," end ) of
                ( [ x0, y0 ], [ x1, y1 ] ) ->
                    if x0 == x1 then
                        Just $ Vertical (Point x0 y0) (y1 - y0)

                    else
                        if y0 == y1 then
                            Just $ Horizontal (Point x0 y0) (x1 - x0)

                        else
                            Nothing

                _ ->
                    error "invalid line input!"

        _ ->
            error "invalid line input!"


data Grid
    = Grid (Map Point Int)


data Point
    = Point { x :: Int, y :: Int }
    deriving (Ord, Eq, Show)


emptyGrid :: Grid
emptyGrid =
    Grid Map.empty


drawLine :: Line -> Grid -> Grid
drawLine line grid =
    List.foldr apply grid (points line)


points :: Line -> [Point]
points (Horizontal (Point x y) length) =
    fmap (\x -> Point x y) [x, x + signum length .. x + length]
points (Vertical (Point x y) length) =
    fmap (Point x) [y, y + signum length .. y + length]

apply :: Point -> Grid -> Grid
apply point (Grid grid) =
    Grid (Map.insert point (fromMaybe 0 (Map.lookup point grid) + 1) grid)

overlappingPoints :: Grid -> Int
overlappingPoints (Grid points) =
    List.length (List.filter (> 1) (Map.elems points))
