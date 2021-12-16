module AdventOfCode.Y2021.Chiton
    ( day
    ) where

import Data.Function
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified AdventOfCode
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Chiton" part1 part2
    where
        part1 =
            lowestRisk . parse

        part2 =
            const 0


parse :: String -> [[Int]]
parse =
    fmap (fmap (read . pure)) . lines


data Position
    = Position { _i :: Int, _j :: Int }
    deriving (Eq, Ord, Show)


lowestRisk :: [[Int]] -> Int
lowestRisk grid =
    let
        unvisited =
            Set.fromList $ map fst $ concat $ withPositions grid

        risks =
            Map.fromList [ ( Position 0 0, 0 ) ]
    in
    risk (Position (length grid - 1) (length grid - 1)) $
        shortestRisks unvisited risks grid


shortestRisks :: Set Position -> Map Position Int -> [[Int]] -> Map Position Int
shortestRisks unvisited risks grid
    | Set.null unvisited =
        risks
    | otherwise =
        let
            current =
                List.minimumBy (compare `on` (flip risk) risks) $
                    Set.toList unvisited

            newUnvisited =
                Set.delete current unvisited

            currentRisk =
                risk current risks

            unvisitedNeighbors =
                filter (flip Set.member unvisited) $
                    neighbors current

            newRisks =
                foldr (reduceRisk currentRisk grid) risks unvisitedNeighbors
        in
        shortestRisks newUnvisited newRisks grid
    where
        reduceRisk currentRisk grid p@(Position i j) risks
            | i >= 0 && i < length grid && j >= 0 && j < length grid =
                let
                    newRisk =
                        currentRisk + (grid !! i !! j)
                in
                if newRisk < risk p risks then
                    Map.insert p newRisk risks

                else
                    risks
            | otherwise =
                risks


withPositions :: [[Int]] -> [[( Position, Int )]]
withPositions =
    fmap (\( i, row ) -> fmap (\( j, risk ) -> ( Position i j, risk )) row)
        . zip [0..] . fmap (zip [0..])


risk :: Position -> Map Position Int -> Int
risk position =
    Maybe.fromMaybe maxBound . Map.lookup position


neighbors :: Position -> [Position]
neighbors (Position i j) =
    let
        moves =
            [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
    in
    [ Position (i + ni) (j + nj) | ( ni, nj ) <- moves ]
