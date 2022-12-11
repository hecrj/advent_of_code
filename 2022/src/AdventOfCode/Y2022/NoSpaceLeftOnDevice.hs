{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
module AdventOfCode.Y2022.NoSpaceLeftOnDevice
    ( day
    ) where

import qualified AdventOfCode
import qualified Data.List.Split as List


day :: AdventOfCode.Day
day =
    AdventOfCode.day "No Space Left On Device" part1 part2
    where
        part1 =
            sum . filter (<= 100000) . directorySizes . parse

        part2 _ =
            0


data Directory
    = Directory { name_ :: String, directories_ :: [Directory], files_ :: [File] }
    deriving Show


parse :: String -> Directory
parse =
    fst
        . run (Directory "/" [] [])
            . map parseCommand
                . drop 1
                    . filter ((/=) "")
                        . List.splitOn "$ "


directorySizes :: Directory -> [Int]
directorySizes directory@(Directory _ directories _) =
    directorySize directory : concatMap directorySizes directories


directorySize :: Directory -> Int
directorySize (Directory _ directories files) =
    sum (map size_ files) + sum (map directorySize directories)


data File
    = File { name_ :: String, size_ :: Int }
    deriving Show


parseFile :: String -> File
parseFile input =
    case words input of
        [ size, name ] ->
            File name (read size)

        _ ->
            error "invalid file!"


data Command
    = Cd Target
    | Ls [File]
    deriving Show


data Target
    = Parent
    | Child String
    deriving Show


parseCommand :: String -> Command
parseCommand input =
    case lines input of
        [ command ] ->
            case words command of
                [ "cd", ".." ] ->
                    Cd Parent

                [ "cd", name ] ->
                    Cd (Child name)

                _ ->
                    error "invalid command without output!"

        "ls" : output ->
            Ls $ map parseFile $ filter isFile output

        _ ->
            error "invalid command!"
    where
        isFile =
            (/=) [ "dir" ] . take 1 . List.splitOn " "


run :: Directory -> [Command] -> ( Directory, [Command] )
run directory [] =
    ( directory, [] )
run (Directory name directories _) (Ls files : commands) =
    run (Directory name directories files) commands
run (Directory name directories files) (Cd (Child childName) : commands) =
    let
        ( child, commands' ) =
            run (Directory childName [] []) commands
    in
    run (Directory name (directories ++ [ child ]) files) commands'
run directory (Cd Parent : commands) =
    ( directory, commands )
