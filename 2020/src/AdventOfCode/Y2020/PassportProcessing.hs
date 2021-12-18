module AdventOfCode.Y2020.PassportProcessing
    ( day
    ) where

import Data.Set as Set (Set)
import qualified AdventOfCode
import qualified Data.List.Split as List
import qualified Data.Set as Set


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Passport Processing" part1 part2
    where
        part1 =
            length . filter isValid . fmap parseFields . List.splitOn "\n\n"

        part2 =
            const 0


data Field
    = BirthYear
    | IssueYear
    | ExpirationYear
    | Height
    | HairColor
    | EyeColor
    | PassportId
    | CountryId
    deriving (Eq, Ord)


parseFields :: String -> Set Field
parseFields =
    Set.fromList . fmap parseField . concat . fmap (List.splitOn " ") . lines
    where
        parseField input =
            case List.splitOn ":" input of
                [ "byr", _ ] ->
                    BirthYear

                [ "iyr", _ ] ->
                    IssueYear

                [ "eyr", _ ] ->
                    ExpirationYear

                [ "hgt", _ ] ->
                    Height

                [ "hcl", _ ] ->
                    HairColor

                [ "ecl", _ ] ->
                    EyeColor

                [ "pid", _ ] ->
                    PassportId

                [ "cid", _ ] ->
                    CountryId

                _ ->
                    error "invalid field!"


isValid :: Set Field -> Bool
isValid fields
    | Set.size fields > 7 =
        True
    | Set.size fields == 7 =
        not $ Set.member CountryId fields
    | otherwise =
        False
