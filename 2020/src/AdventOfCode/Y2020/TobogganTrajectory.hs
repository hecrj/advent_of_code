module AdventOfCode.Y2020.TobogganTrajectory
    ( day
    ) where

import qualified AdventOfCode


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Toboggan Trajectory" part1 part2
    where
        part1 =
            length . filter isTree . cellsUntilBottom 1 3 . parse

        part2 input =
            let
                map =
                    parse input
            in
            product $
                fmap (length . filter isTree) $
                    fmap ($ map) $
                        fmap (uncurry cellsUntilBottom)
                            [ ( 1, 1 )
                            , ( 1, 3 )
                            , ( 1, 5 )
                            , ( 1, 7 )
                            , ( 2, 1 )
                            ]


data Map
    = Map [[Cell]]


parse :: String -> Map
parse =
    Map . map parseRow . lines
    where
        parseRow =
            map parseCell


cellsUntilBottom :: Int -> Int -> Map -> [Cell]
cellsUntilBottom =
    move 0 0
    where
        move i j di dj map@(Map grid)
            | i >= length grid =
                []
            | otherwise =
                let
                    row =
                        grid !! i
                in
                row !! (j `mod` length row) : move (i + di) (j + dj) di dj map


data Cell
    = Free
    | Tree


parseCell :: Char -> Cell
parseCell '.' =
    Free
parseCell '#' =
    Tree
parseCell _ =
    error "invalid cell!"


isTree :: Cell -> Bool
isTree Free =
    False
isTree Tree =
    True
