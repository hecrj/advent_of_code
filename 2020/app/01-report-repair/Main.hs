module Main where

import Data.Set (Set)
import qualified Data.Set as Set


main :: IO ()
main = do
    report <- fmap (fmap read . lines) getContents

    case findPair 2020 report of
        Just ( a, b ) ->
            putStrLn $ show (a * b)

        Nothing ->
            error "no pair found!"


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
