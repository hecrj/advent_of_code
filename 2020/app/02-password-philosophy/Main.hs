module Main where

import qualified Data.List.Split as List


main :: IO ()
main = do
    list <- fmap (map parse . lines) getContents
    putStrLn $ show $ length $ filter (uncurry validate) list


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
    let
        occurrences =
            length $ filter ((==) letter) password
    in
    min <= occurrences && occurrences <= max


data Password
    = Password String
