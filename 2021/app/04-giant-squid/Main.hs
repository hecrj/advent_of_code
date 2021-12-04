module Main where

import Data.List
import Data.List.Split


main :: IO ()
main = do
    (inputNumbers : inputBoards) <- fmap (filter (not . null) . splitOn "\n\n") getContents

    let
        numbers =
            map read (splitOn "," inputNumbers) :: [Int]

        boards =
            map parseBoard inputBoards

    case bingo boards numbers of
        Just bingo -> do
            putStrLn $ show bingo
            putStrLn $ show (score bingo)

        Nothing ->
            error "no bingo!"

    case bingoLast boards numbers of
        Just bingo -> do
            putStrLn $ show bingo
            putStrLn $ show (score bingo)

        Nothing ->
            error "no bingo!"


data Board
    = Board [[Cell]]
    deriving Show


data Cell
    = Unmarked Int
    | Marked Int
    deriving Show


isMarked :: Cell -> Bool
isMarked (Unmarked _) =
    False
isMarked (Marked _) =
    True


parseBoard :: String -> Board
parseBoard =
    Board . map (map (Unmarked . read) . filter (not . null) . splitOn " ") . lines


data Bingo
    = Bingo
        { winner :: Board
        , lastNumber :: Int
        }
    deriving Show


bingo :: [Board] -> [Int] -> Maybe Bingo
bingo _ [] =
    Nothing
bingo boards (number : rest) =
    let
        newBoards =
            map (mark number) boards
    in
    case find isWinner newBoards of
        Just winner ->
            Just (Bingo winner number)

        Nothing ->
            bingo newBoards rest


bingoLast :: [Board] -> [Int] -> Maybe Bingo
bingoLast _ [] =
    Nothing
bingoLast boards (number : rest) =
    let
        newBoards =
            map (mark number) boards
    in
    case filter (not . isWinner) newBoards of
        [] ->
            case find isWinner newBoards of
                Just winner ->
                    Just (Bingo winner number)

                Nothing ->
                    error "left boards win at the same time!"

        losingBoards ->
            bingoLast losingBoards rest


mark :: Int -> Board -> Board
mark number (Board cells) =
    Board (map (map replace) cells)
    where
        replace (Unmarked n)
            | n == number =
                Marked n
            | otherwise =
                Unmarked n
        replace (Marked n) =
            Marked n


isWinner :: Board -> Bool
isWinner (Board cells) =
    any (all isMarked) cells || any (all isMarked) (transpose cells)


score :: Bingo -> Int
score (Bingo (Board cells) lastNumber) =
    sum unmarkedNumbers * lastNumber
    where
        unmarkedNumbers =
            map number (filter (not . isMarked) (concat cells))

        number (Unmarked n) =
            n
        number (Marked n) =
            n
