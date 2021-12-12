module AdventOfCode.Y2021.BinaryDiagnostic where

import Data.Bits
import Data.List
import qualified AdventOfCode


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Binary Diagnostic" part1 part2


part1 :: String -> Int
part1 input =
    let
        report =
            parse input

        gammaRate =
            computeGammaRate report

        epsilonRate =
            map inverse gammaRate
    in
    toDecimal gammaRate * toDecimal epsilonRate


part2 :: String -> Int
part2 input =
    let
        report =
            parse input

        oxygenGeneratorRating =
            computeByBitCriteria oxygenGeneratorCriteria report

        co2ScrubberRating =
            computeByBitCriteria co2ScrubberCriteria report

        oxygenGeneratorCriteria =
            mostCommonBit One

        co2ScrubberCriteria =
            inverse . oxygenGeneratorCriteria
    in
    toDecimal oxygenGeneratorRating * toDecimal co2ScrubberRating


parse :: String -> [[Bit]]
parse =
    fmap parseBits . lines


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
    map (mostCommonBit tie) . transpose
    where
        tie =
            error "Invalid report!"


computeByBitCriteria :: ([Bit] -> Bit) -> [[Bit]] -> [Bit]
computeByBitCriteria =
    helper 0
    where
        helper _ _ [] =
            error "Run out of values!"
        helper _ _ [ n ] =
            n
        helper i criteria list =
            let
                selectedBit =
                    criteria ((transpose list) !! i)
            in
            helper (i + 1) criteria (filter (\n -> n !! i == selectedBit) list)


mostCommonBit :: Bit -> [Bit] -> Bit
mostCommonBit tie bits
    | length (ones bits) == length (zeroes bits) =
        tie
    | length (ones bits) > length (zeroes bits) =
        One
    | otherwise =
        Zero
    where
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
