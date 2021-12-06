module Main where

import Data.Maybe
import qualified Data.List.Split as List


main :: IO ()
main = do
    lanternfish <- fmap (fmap parseLanternfish . List.splitOn ",") getContents

    let
        lanternfishByDay =
            iterate simulate lanternfish

    putStrLn $ show $ length (lanternfishByDay !! 80)


data Lanternfish
    = Lanternfish Int
    deriving Show


parseLanternfish :: String -> Lanternfish
parseLanternfish =
    Lanternfish . read


live :: Lanternfish -> ( Lanternfish, Maybe Lanternfish )
live (Lanternfish 0) =
    ( Lanternfish 6, Just (Lanternfish 8) )
live (Lanternfish n) =
    ( Lanternfish (n - 1), Nothing )


simulate :: [Lanternfish] -> [Lanternfish]
simulate =
    merge . unzip . fmap live
    where
        merge ( fish, births ) =
            concat [ fish, catMaybes births ]
