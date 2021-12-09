module Main where

import Data.Maybe
import Debug.Trace
import Relude.List


main :: IO ()
main = do
    heightMap <- fmap parseHeightMap getContents
    putStrLn $ show heightMap

    let
        lowPointsScore =
            sum $ fmap (+ 1) (lowPoints heightMap)

    putStrLn $ show lowPointsScore


data HeightMap
    = HeightMap [[Int]]
    deriving Show


parseHeightMap :: String -> HeightMap
parseHeightMap =
    HeightMap . fmap parseRow . lines
    where
        parseRow =
            fmap (read . pure)


lowPoints :: HeightMap -> [Int]
lowPoints map@(HeightMap grid) =
    [
        height
        | ( i, row ) <- enumerate grid
        , ( j, height ) <- enumerate row
        , filter (<= height) (adjacentPoints map i j) == []
    ]
    where
        enumerate =
            zip [0..]


adjacentPoints :: HeightMap -> Int -> Int -> [Int]
adjacentPoints (HeightMap grid) i j =
    let
        moves =
            [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
    in
    catMaybes $
        fmap
            (\( mi, mj ) -> do
                row <- grid !!? (i + mi)
                row !!? (j + mj)
            )
            moves


debug :: Show a => a -> a
debug a =
    traceShow a a
