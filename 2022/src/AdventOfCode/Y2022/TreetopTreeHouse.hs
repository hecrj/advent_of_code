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
            maximum . scenicScores . parse


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
    case map maximum $ filter (not . List.null) $ pathsToEdges forest position of
        [] ->
            True

        paths ->
            tree > minimum paths


scenicScores :: Forest -> [Int]
scenicScores forest@(Forest trees) =
    concatMap (fmap (scenicScore forest)) $ withPositions trees


scenicScore :: Forest -> ( Position, Tree ) -> Int
scenicScore forest ( position, tree ) =
    product $ fmap (length . takeWhileInclusive (< tree)) $ pathsToEdges forest position
    where
        takeWhileInclusive _ [] =
            []
        takeWhileInclusive p (x : xs) =
            x : if p x then takeWhileInclusive p xs else []


pathsToEdges :: Forest -> Position -> [[Tree]]
pathsToEdges (Forest trees) (Position i j) =
    [ reverse $ take j (trees !! i)
    , drop (j + 1) (trees !! i)
    , reverse $ take i $ (List.transpose trees) !! j
    , drop (i + 1) $ (List.transpose trees) !! j
    ]


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
