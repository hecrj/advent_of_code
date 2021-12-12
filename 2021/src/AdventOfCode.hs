module AdventOfCode
    ( Day
    , Solution
    , Year
    , day
    , year
    , run
    ) where

import Data.Char
import Data.Maybe
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.IO as IO


data Day
    = Day { title :: String, _solution1 :: Solution, _solution2 :: Solution }


type Solution = String -> Int


day :: String -> Solution -> Solution -> Day
day =
    Day


data Year
    = Year { _number :: Int, _days :: [Day] }


year :: Int -> [Day] -> Year
year =
    Year


run :: Year -> IO ()
run (Year number days) = do
    let
        title =
            "Advent of Code " ++ show number

        separator =
            take (length title) (repeat '=')

    putStrLn separator
    putStrLn title
    putStrLn separator
    runDays days


runDays :: [Day] -> IO ()
runDays days = do
    let
        maxTitle =
            length $ maximum (fmap title days)

        columns =
            [ "Day"
            , padRight maxTitle ' ' "Name"
            , padRight 10 ' ' "Part 1"
            , padRight 15 ' ' "Part 2"
            ]

        header =
            concat $ List.intersperse " | " columns

        separator =
            concat $ List.intersperse " | " $ map (flip replicate '-' . length) columns

    putStrLn header
    putStrLn separator
    outputs <- sequence $ map (uncurry runDay) $ zip [1..] days
    sequence_ $ map (uncurry (putOutput maxTitle)) $ zip [1..] outputs


data Output
    = Output
        { _title :: String
        , _example :: Maybe ( Int, Int )
        , _input :: ( Int, Int )
        }


putOutput :: Int -> Int -> Output -> IO ()
putOutput titlePadding number (Output title _example input) =
    let
        day =
            padLeft 2 '0' (show number)

        paddedTitle =
            padRight titlePadding ' ' title

        row =
            concat $
                List.intersperse " | "
                    [ ' ' : day
                    , paddedTitle
                    , padLeft 10 ' ' $ show (fst input)
                    , padLeft 15 ' ' $ show (snd input)
                    ]
    in
    putStrLn row


runDay :: Int -> Day -> IO Output
runDay number (Day title solution1 solution2) = do
    example <- run' "example"
    input <- run' "input"
    return $ Output title example $ fromMaybe (error ("input not found for: " ++ title)) input
    where
        run' :: String -> IO (Maybe ( Int, Int ))
        run' directory = do
            let
                filename =
                    concat
                        [ padLeft 2 '0' (show number)
                        , "-"
                        , map replaceSpace (map toLower title)
                        , ".txt"
                        ]

                inputPath =
                    directory ++ "/" ++ filename

            inputFileExists <- Directory.doesFileExist inputPath

            if inputFileExists then
                do
                    input <- IO.readFile inputPath
                    return $ Just ( solution1 input, solution2 input )

            else
                return Nothing
        replaceSpace ' ' =
            '-'
        replaceSpace c =
            c


padLeft :: Int -> Char -> String -> String
padLeft n c s =
    replicate (n - length s) c ++ s


padRight :: Int -> Char -> String -> String
padRight n c s =
    s ++ replicate (n - length s) c
