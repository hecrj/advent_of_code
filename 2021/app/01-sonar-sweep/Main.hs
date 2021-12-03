module Main where

import Data.List as List


main :: IO ()
main = do
    measurements <- fmap (fmap read . lines) getContents :: IO [Integer]
    putStrLn (show (countIncreases measurements))
    putStrLn (show (countWindowIncreases measurements))


countIncreases :: [Integer] -> Integer
countIncreases [] =
    0
countIncreases [ _ ] =
    0
countIncreases (a : b : rest)
    | b > a =
        1 + countIncreases (b : rest)
    | otherwise =
        countIncreases (b : rest)


countWindowIncreases :: [Integer] -> Integer
countWindowIncreases list =
    countIncreases $
        concat $
            List.transpose [ window 0, window 1, window 2, window 3 ]
    where
        window n =
            sum3 (drop n list)

        sum3 (a : b : c : _ : xs) =
            ((a + b + c) : sum3 xs)
        sum3 [ a, b, c ] =
            [ a + b + c ]
        sum3 _ =
            []
