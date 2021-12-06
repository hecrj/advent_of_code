module Main where

import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.List.Split as List
import qualified Data.Map.Strict as Map


main :: IO ()
main = do
    fish <- fmap (fmap parseLanternfish . List.splitOn ",") getContents

    let
        oceanByDay =
            iterate simulate (newOcean fish)

    putStrLn $ show $ countFish (oceanByDay !! 80)
    putStrLn $ show $ countFish (oceanByDay !! 256)


data Lanternfish
    = Lanternfish Int
    deriving (Eq, Show)


parseLanternfish :: String -> Lanternfish
parseLanternfish =
    Lanternfish . read


data Ocean
    = Ocean (Map Int Int)


newOcean :: [Lanternfish] -> Ocean
newOcean fish =
    Ocean (Map.fromList (map count [0..8]))
    where
        count day =
            ( day, length (filter ((==) (Lanternfish day)) fish) )


simulate :: Ocean -> Ocean
simulate (Ocean current) =
    let
        shifted =
            Map.fromList (map shift [1..8])

        births =
            fromMaybe 0 (Map.lookup 0 current)

        new =
            Map.insert 8 births $
                Map.insert 6 (fromMaybe 0 (Map.lookup 6 shifted) + births) shifted
    in
    Ocean new
    where
        shift day =
            ( day - 1, fromMaybe 0 (Map.lookup day current) )


countFish :: Ocean -> Int
countFish (Ocean map) =
    sum (Map.elems map)
