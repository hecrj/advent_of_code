module AdventOfCode.Y2021.GiantSquid
    ( day
    ) where

import Data.List
import Data.List.Split
import Data.Maybe
import qualified AdventOfCode


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Giant Squid" part1 part2
    where
        part1 =
            run bingo

        part2 =
            run bingoLast

        run f =
            fromMaybe (error "no bingo!") . fmap score . uncurry f . parse

        parse input =
            case filter (not . null) $ splitOn "\n\n" input of
                (inputNumbers : inputBoards) ->
                    let
                        numbers =
                            map read (splitOn "," inputNumbers) :: [Int]

                        boards =
                            map parseBoard inputBoards
                    in
                    ( boards, numbers )

                _ ->
                    error "invalid input!"


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
