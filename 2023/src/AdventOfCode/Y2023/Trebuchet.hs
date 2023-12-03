module AdventOfCode.Y2023.Trebuchet (day) where

import qualified AdventOfCode
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Internal.Search as Search


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Trebuchet" part1 part2


part1 :: String -> Int
part1 =
    sum . fmap calibration . lines


part2 :: String -> Int
part2 =
    sum . fmap (accurateCalibration . Text.pack) . lines


calibration :: String -> Int
calibration line =
    let
        digits = filter Char.isDigit line
        firstDigit = Char.digitToInt $ head digits
        lastDigit = Char.digitToInt $ head (reverse digits)
    in
        firstDigit * 10 + lastDigit


accurateCalibration :: Text -> Int
accurateCalibration line =
    let
        foundDigits = findDigits digits line
    in
        head foundDigits * 10 + last foundDigits
  where
    digits =
        [ ("one", 1)
        , ("two", 2)
        , ("three", 3)
        , ("four", 4)
        , ("five", 5)
        , ("six", 6)
        , ("seven", 7)
        , ("eight", 8)
        , ("nine", 9)
        ]

    findDigits digits line =
        fmap snd $ List.sortOn fst $ concatMap (uncurry digitIndices) digits
      where
        digitIndices target value =
            let
                targetIndices = Search.indices target line
                characterIndices = Search.indices (Text.singleton (Char.intToDigit value)) line
            in
                fmap (,value) (targetIndices ++ characterIndices)
