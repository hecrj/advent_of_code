module AdventOfCode.Y2021.HydrothermalVenture
    ( day
    ) where

import Data.List as List
import Data.List.Split
import Data.Map.Strict as Map
import Data.Maybe
import qualified AdventOfCode


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Hydrothermal Venture" part1 part2
    where
        part1 =
            overlappingPoints
                . gridFromLines
                    . List.filter (\p -> isHorizontal p || isVertical p)
                        . parse

        part2 =
            overlappingPoints . gridFromLines . parse

        parse =
            catMaybes . fmap parseLine . lines


data Line
    = Horizontal { start :: Point, length :: Int }
    | Vertical { start :: Point, length :: Int }
    | Diagonal { start :: Point, end :: Point }
    deriving Show


isHorizontal :: Line -> Bool
isHorizontal (Horizontal _ _) =
    True
isHorizontal _ =
    False


isVertical :: Line -> Bool
isVertical (Vertical _ _) =
    True
isVertical _ =
    False


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
                            Just $ Diagonal (Point x0 y0) (Point x1 y1)

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


gridFromLines :: [Line] -> Grid
gridFromLines =
    List.foldr drawLine emptyGrid


drawLine :: Line -> Grid -> Grid
drawLine line grid =
    List.foldr apply grid (points line)


points :: Line -> [Point]
points (Horizontal (Point x y) length) =
    fmap (\x -> Point x y) (enumFromThenTo x (x + signum length) (x + length))
points (Vertical (Point x y) length) =
    fmap (Point x) (enumFromThenTo y (y + signum length) (y + length))
points (Diagonal (Point x0 y0) (Point x1 y1)) =
    let
        distanceX =
            x1 - x0

        distanceY =
            y1 - y0

        diagonal =
            List.zip
                (enumFromThenTo x0 (x0 + signum distanceX) (x0 + distanceX))
                (enumFromThenTo y0 (y0 + signum distanceY) (y0 + distanceY))
    in
    fmap (\( x, y ) -> Point x y) diagonal


apply :: Point -> Grid -> Grid
apply point (Grid grid) =
    Grid (Map.insert point (fromMaybe 0 (Map.lookup point grid) + 1) grid)


overlappingPoints :: Grid -> Int
overlappingPoints (Grid points) =
    List.length (List.filter (> 1) (Map.elems points))
