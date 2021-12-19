module AdventOfCode
    ( Day
    , Solution
    , Year
    , day
    , year
    , run
    , debug
    , pairs
    ) where

import Data.Char
import Data.Maybe
import qualified Data.List as List
import qualified Debug.Trace as Debug
import qualified System.Directory as Directory
import qualified System.Environment as Environment
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
    args <- fmap (fmap read) Environment.getArgs

    let
        title =
            "Advent of Code " ++ show number

        separator =
            take (length title) (repeat '=')

        selection =
            if List.null args then
                zip [1..] days

            else
                filter (flip List.elem args . fst) $ zip [1..] days

    putStrLn separator
    putStrLn title
    putStrLn separator
    runDays selection


runDays :: [( Int, Day )] -> IO ()
runDays days = do
    let
        maxTitle =
            maximum $ fmap (length . title . snd) days

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
    outputs <- sequence $ map (uncurry runDay) days
    sequence_ $ map (putOutput maxTitle) outputs


data Output
    = Output
        { _day :: Int
        , _title :: String
        , _example :: Maybe ( Int, Int )
        , _input :: ( Int, Int )
        }


putOutput :: Int -> Output -> IO ()
putOutput titlePadding (Output number title _example input) =
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
    return $ Output number title example $ fromMaybe (error ("input not found for: " ++ title)) input
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


debug :: Show a => a -> a
debug a =
    Debug.traceShow a a


pairs :: [a] -> [( a, a )]
pairs xs =
    let
        pairs' =
            [ ( x, y ) | (x : rest) <- List.tails xs, y <- rest ]
    in
    concat $ fmap (\( a, b ) -> [ ( a, b ), ( b, a ) ]) pairs'
