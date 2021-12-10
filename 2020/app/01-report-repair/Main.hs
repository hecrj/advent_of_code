module Main where

import Data.Set (Set)
import qualified Data.List as List
import qualified Data.Set as Set


main :: IO ()
main = do
    report <- fmap (fmap read . lines) getContents

    case findPair 2020 report of
        Just ( a, b ) ->
            putStrLn $ show (a * b)

        Nothing ->
            error "no pair found!"

    case findTriplet 2020 report of
        Just ( a, b, c ) ->
            putStrLn $ show (a * b * c)

        Nothing ->
            error "no triplet found!"


findPair :: Int -> [Int] -> Maybe ( Int, Int )
findPair =
    findPair' Set.empty
    where
        findPair' visited target [] =
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
