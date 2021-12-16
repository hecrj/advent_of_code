module AdventOfCode.Y2021.Chiton
    ( day
    ) where

import Data.Function
import Data.Map.Strict (Map)
import Data.PQueue.Prio.Min (MinPQueue)
import qualified AdventOfCode
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.PQueue.Prio.Min as MinPQueue


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
    deriving (Eq, Ord, Show)


lowestRisk :: [[Int]] -> Int
lowestRisk grid =
    let
        unvisited =
            MinPQueue.singleton 0 (Position 0 0)

        risks =
            Map.fromList [ ( Position 0 0, 0 ) ]
    in
    risk (Position (length grid - 1) (length grid - 1)) $
        shortestRisks unvisited risks grid


shortestRisks :: MinPQueue Int Position -> Map Position Int -> [[Int]] -> Map Position Int
shortestRisks unvisited risks grid
    | MinPQueue.null unvisited =
        risks
    | otherwise =
        let
            ( currentRisk, current ) =
                MinPQueue.findMin unvisited

            newUnvisited =
                MinPQueue.deleteMin unvisited

            unvisitedNeighbors =
                neighbors current

            ( finalUnvisited, newRisks ) =
                foldr (reduceRisk currentRisk grid) ( newUnvisited, risks ) unvisitedNeighbors
        in
        shortestRisks finalUnvisited newRisks grid
    where
        reduceRisk currentRisk grid p@(Position i j) ( unvisited, risks )
            | i >= 0 && i < length grid && j >= 0 && j < length grid =
                let
                    newRisk =
                        currentRisk + (grid !! i !! j)
                in
                if newRisk < risk p risks then
                    let
                        newUnvisited =
                            if List.elem p (MinPQueue.elems unvisited) then
                                newUnvisited

                            else
                                MinPQueue.insert newRisk p unvisited
                    in
                    ( newUnvisited, Map.insert p newRisk risks )

                else
                    ( unvisited, risks )
            | otherwise =
                ( unvisited, risks )


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
