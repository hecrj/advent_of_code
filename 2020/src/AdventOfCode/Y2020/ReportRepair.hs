module AdventOfCode.Y2020.ReportRepair
    ( day
    ) where

import Data.Maybe
import qualified AdventOfCode
import qualified Data.List as List
import qualified Data.Set as Set


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Report Repair" part1 part2
    where
        part1 =
            uncurry (*)
                . fromMaybe (error "pair not found!")
                    . findPair 2020 . map read . lines

        part2 =
            product
                . fromMaybe (error "tripler not found!")
                    . findTriplet 2020 . map read . lines

        product ( a, b, c ) =
            a * b * c


findPair :: Int -> [Int] -> Maybe ( Int, Int )
findPair =
    findPair' Set.empty
    where
        findPair' _ _ [] =
            Nothing
        findPair' visited target (candidate : rest)
            | Set.member candidate visited =
                Just ( target - candidate, candidate )
            | otherwise =
                findPair' (Set.insert (target - candidate) visited) target rest


findTriplet :: Int -> [Int] -> Maybe ( Int, Int, Int )
findTriplet target list =
    List.find (\( a, b, c ) -> a + b + c == target)
        [
            ( a, b, c )
            | a <- list
            , b <- list
            , c <- list
            , a /= b && b /= c && c /= a
        ]
