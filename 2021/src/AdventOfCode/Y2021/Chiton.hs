{-# LANGUAGE DeriveGeneric #-}
module AdventOfCode.Y2021.Chiton
    ( day
    ) where

import Data.Hashable
import GHC.Generics (Generic)
import qualified AdventOfCode
import qualified AdventOfCode.Path as Path
import qualified Data.Maybe as Maybe


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Chiton" part1 part2
    where
        part1 =
            lowestRisk . parse

        part2 =
            lowestRisk . extend . parse


parse :: String -> [[Int]]
parse =
    fmap (fmap (read . pure)) . lines


extend :: [[Int]] -> [[Int]]
extend grid =
    concat $ fmap (extendGrid grid) [0..4]
    where
        extendGrid grid i =
            fmap (extendRow i) grid

        extendRow i row =
            concat $ fmap (generateRow i row) [0..4]

        generateRow i row j =
            fmap
                (\risk ->
                    if risk + i + j > 9 then
                        (risk + i + j) `mod` 9

                    else
                        risk + i + j
                )
                row


data Position
    = Position { _i :: Int, _j :: Int }
    deriving (Eq, Ord, Show, Generic)


instance Hashable Position where


lowestRisk :: [[Int]] -> Int
lowestRisk grid =
    let
        start =
            Position 0 0

        end =
            Position (length grid - 1) (length grid - 1)
    in
    fst . Maybe.fromJust $
        Path.search start end
            (neighbors grid)
            (manhattan end)


manhattan :: Position -> Path.Heuristic Position
manhattan (Position jEnd iEnd) (Position i j) =
    abs (iEnd - i) + abs (jEnd - j)


neighbors :: [[Int]] -> Path.Movement Position
neighbors grid (Position i j) =
    let
        moves =
            [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
    in
    fmap withCost $
        filter inBounds
            [
                Position (i + ni) (j + nj)
                | ( ni, nj ) <- moves
            ]
    where
        inBounds (Position i j) =
            i >= 0 && j >= 0 && i < length grid && j < length grid

        withCost p@(Position i j) =
            ( p, grid !! i !! j )
