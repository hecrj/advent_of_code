module AdventOfCode.Y2022.TreetopTreeHouse
    ( day
    ) where

import qualified AdventOfCode
import qualified Data.List as List


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Treetop Tree House" part1 part2
    where
        part1 =
            countVisible . parse

        part2 =
            const 0


newtype Forest
    = Forest [[Tree]]


parse :: String -> Forest
parse =
    Forest . fmap (map parseTree) . lines


countVisible :: Forest -> Int
countVisible forest@(Forest trees) =
    length $ concatMap (filter (isVisible forest)) $ withPositions trees


isVisible :: Forest -> ( Position, Tree ) -> Bool
isVisible forest ( position, tree ) =
    tree > (minimum $ map maximum $ pathsToEdges forest position)


pathsToEdges :: Forest -> Position -> [[Tree]]
pathsToEdges (Forest trees) (Position i j) =
    map visibleEdge
        [ take j (trees !! i)
        , drop (j + 1) (trees !! i)
        , take i $ (List.transpose trees) !! j
        , drop (i + 1) $ (List.transpose trees) !! j
        ]
    where
        visibleEdge [] =
            [ Tree (-1) ]
        visibleEdge x =
            x


newtype Tree
    = Tree Int
    deriving (Show, Eq, Ord)


parseTree :: Char -> Tree
parseTree =
    Tree . read . pure


data Position
    = Position { i_ :: Int, j_ :: Int }
    deriving (Eq, Ord, Show)


withPositions :: [[a]] -> [[( Position, a )]]
withPositions =
    fmap (\( i, row ) -> fmap (\( j, a ) -> ( Position i j, a )) row)
        . zip [0..] . fmap (zip [0..])
