module Main where

import Data.Bits
import Data.List


main :: IO ()
main = do
    report <- fmap (fmap parseBits . lines) getContents

    let
        gammaRate =
            computeGammaRate report

        epsilonRate =
            map inverse gammaRate

    putStrLn $ show (toDecimal gammaRate * toDecimal epsilonRate)


data Bit
    = Zero
    | One
    deriving (Eq, Show)


parseBits :: String -> [Bit]
parseBits =
    map parseBit
    where
        parseBit '0' =
            Zero
        parseBit '1' =
            One
        parseBit _ =
            error "Invalid bit!"


computeGammaRate :: [[Bit]] -> [Bit]
computeGammaRate =
    map mostCommonBit . transpose
    where
        mostCommonBit bits
            | length (ones bits) == length (zeroes bits) =
                error "Invalid report!"
            | length (ones bits) > length (zeroes bits) =
                One
            | otherwise =
                Zero

        ones =
            filter ((==) One)

        zeroes =
            filter ((==) Zero)


inverse :: Bit -> Bit
inverse Zero =
    One
inverse One =
    Zero


toDecimal :: [Bit] -> Int
toDecimal =
    foldl enableBit zeroBits . zip [0..] . reverse
    where
        enableBit decimal ( _, Zero ) =
            decimal
        enableBit decimal ( i, One ) =
            setBit decimal i
