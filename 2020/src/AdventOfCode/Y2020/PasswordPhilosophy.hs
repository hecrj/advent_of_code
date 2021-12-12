module AdventOfCode.Y2020.PasswordPhilosophy where

import qualified AdventOfCode
import qualified Data.List.Split as List


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Password Philosophy" part1 part2
    where
        part1 =
            length . filter (uncurry validateOld) . map parse . lines

        part2 =
            length . filter (uncurry validate) . map parse . lines


parse :: String -> ( Policy, Password )
parse input =
    case List.splitOn ": " input of
        [ policy, password ] ->
            ( parsePolicy policy, Password password )

        _ ->
            error "invalid input!"


data Policy
    = Policy { min :: Int, max :: Int, letter :: Char }


parsePolicy :: String -> Policy
parsePolicy input =
    case List.splitOn " " input of
        [ constraints, [ letter ] ] ->
            case fmap read (List.splitOn "-" constraints) of
                [ min, max ] ->
                    Policy min max letter

                _ ->
                    error "invalid policy!"

        _ ->
            error "invalid policy!"


validate :: Policy -> Password -> Bool
validate (Policy min max letter) (Password password) =
    (/=)
        (password !! (min - 1) == letter)
        (password !! (max - 1) == letter)


validateOld :: Policy -> Password -> Bool
validateOld (Policy min max letter) (Password password) =
    let
        occurrences =
            length $ filter ((==) letter) password
    in
    min <= occurrences && occurrences <= max


data Password
    = Password String
