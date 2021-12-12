module AdventOfCode.Y2021.SmokeBasin where

import Data.Maybe
import Debug.Trace
import Relude.List
import qualified AdventOfCode
import qualified Data.List as List


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Smoke Basin" lowPointsScore basinsScore
    where
        lowPointsScore =
            sum . fmap ((+ 1) . height) . lowPoints . parseHeightMap

        basinsScore =
            foldr (*) 1 . take 3 . reverse . sort . fmap length . basins . parseHeightMap


data HeightMap
    = HeightMap [[Int]]
    deriving Show


data Point
    = Point { i :: Int, j :: Int, height :: Int }
    deriving (Eq, Show)


parseHeightMap :: String -> HeightMap
parseHeightMap =
    HeightMap . fmap parseRow . lines
    where
        parseRow =
            fmap (read . pure)


lowPoints :: HeightMap -> [Point]
lowPoints map@(HeightMap grid) =
    [
        Point i j h
        | ( i, row ) <- enumerate grid
        , ( j, h ) <- enumerate row
        , filter ((<= h) . height) (adjacentPoints map i j) == []
    ]
    where
        enumerate =
            zip [0..]


adjacentPoints :: HeightMap -> Int -> Int -> [Point]
adjacentPoints (HeightMap grid) i j =
    let
        moves =
            [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
    in
    catMaybes $
        fmap
            (\( mi, mj ) -> do
                row <- grid !!? (i + mi)
                height <- row !!? (j + mj)
                Just $ Point (i + mi) (j + mj) height
            )
            moves


basins :: HeightMap -> [[Point]]
basins map@(HeightMap _grid) =
    let
        startingPoints =
            lowPoints map
    in
    fmap (scan map [] . pure) startingPoints


scan :: HeightMap -> [Point] -> [Point] -> [Point]
scan _ _ [] =
    []
scan map visited (p@(Point i j h) : rest) =
    let
        newPoints =
            filter (not . (flip elem) visited) $
                filter (\(Point _ _ height) -> height > h && height < 9)
                    (adjacentPoints map i j)
    in
    p : scan map (p : visited) (List.union rest newPoints)


debug :: Show a => a -> a
debug a =
    traceShow a a
