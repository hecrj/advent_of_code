module AdventOfCode.Y2020.PassportProcessing
    ( day
    ) where

import Data.Map.Strict (Map)
import qualified AdventOfCode
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Map.Strict as Map


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Passport Processing" part1 part2
    where
        part1 =
            length . filter allFieldsPresent . parse

        part2 =
            length . filter isValid . parse


parse :: String -> [Map Field String]
parse =
    fmap parseFields . List.splitOn "\n\n"


data Field
    = BirthYear
    | IssueYear
    | ExpirationYear
    | Height
    | HairColor
    | EyeColor
    | PassportId
    | CountryId
    deriving (Eq, Ord, Show)


parseFields :: String -> Map Field String
parseFields =
    Map.fromList . fmap parseField . concat . fmap (List.splitOn " ") . lines
    where
        parseField input =
            case List.splitOn ":" input of
                [ "byr", value ] ->
                    ( BirthYear, value )

                [ "iyr", value ] ->
                    ( IssueYear, value )

                [ "eyr", value ] ->
                    ( ExpirationYear, value )

                [ "hgt", value ] ->
                    ( Height, value )

                [ "hcl", value ] ->
                    ( HairColor, value )

                [ "ecl", value ] ->
                    ( EyeColor, value )

                [ "pid", value ] ->
                    ( PassportId, value )

                [ "cid", value ] ->
                    ( CountryId, value )

                _ ->
                    error "invalid field!"


allFieldsPresent :: Map Field String -> Bool
allFieldsPresent fields =
    Map.size fields > 7
        || (Map.size fields == 7 && not (Map.member CountryId fields))


isValid :: Map Field String -> Bool
isValid fields =
    allFieldsPresent fields
        && all (uncurry isFieldValid) (Map.toList fields)


isFieldValid :: Field -> String -> Bool
isFieldValid BirthYear =
    isDigits 4 1920 2002
isFieldValid IssueYear =
    isDigits 4 2010 2020
isFieldValid ExpirationYear =
    isDigits 4 2020 2030
isFieldValid Height =
    or
        . flip fmap
            [ measurement "cm" 150 193
            , measurement "in" 59 76
            ]
            . flip ($)
isFieldValid HairColor =
    hexadecimalColor
isFieldValid EyeColor =
    oneOf
        [ "amb"
        , "blu"
        , "brn"
        , "gry"
        , "grn"
        , "hzl"
        , "oth"
        ]
isFieldValid PassportId =
    isDigits 9 minBound maxBound
isFieldValid CountryId =
    const True


isDigits :: Int -> Int -> Int -> String -> Bool
isDigits n min max s =
    let
        number =
            read s
    in
    all Char.isDigit s && length s == n && min <= number && number <= max


measurement :: String -> Int -> Int -> String -> Bool
measurement units min max value =
    let
        number =
            takeWhile Char.isDigit value
    in
    if drop (length number) value == units then
        min <= read number && read number <= max

    else
        False


hexadecimalColor :: String -> Bool
hexadecimalColor ('#' : rest) =
    length rest == 6 && all Char.isHexDigit rest
hexadecimalColor _ =
    False


oneOf :: [String] -> String -> Bool
oneOf =
    flip List.elem
