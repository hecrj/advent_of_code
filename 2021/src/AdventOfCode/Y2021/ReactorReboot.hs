module AdventOfCode.Y2021.ReactorReboot
    ( day
    ) where

import Data.Set (Set)
import qualified AdventOfCode
import qualified Data.List.Split as List
import qualified Data.Set as Set


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Reactor Reboot" part1 part2
    where
        part1 =
            count . initialize . parse

        part2 =
            const 0


parse :: String -> [Step]
parse =
    fmap parseStep . lines


data Reactor
    = Reactor (Set Cube)


initialize :: [Step] -> Reactor
initialize =
    foldr apply (Reactor Set.empty) . filter isInitialization . drop 2 . reverse


apply :: Step -> Reactor -> Reactor
apply (On region) (Reactor enabled) =
    Reactor (Set.union enabled (cubes region))
apply (Off region) (Reactor enabled) =
    Reactor (Set.difference enabled (cubes region))


count :: Reactor -> Int
count (Reactor cubes) =
    Set.size cubes


data Cube
    = Cube Position
    deriving (Eq, Ord)


data Step
    = On Region
    | Off Region
    deriving Show


parseStep :: String -> Step
parseStep input =
    case List.splitOn " " input of
        [ "on", region ] ->
            On $ parseRegion region

        [ "off", region ] ->
            Off $ parseRegion region

        _ ->
            error "invalid step!"


region :: Step -> Region
region (On region) =
    region
region (Off region) =
    region


isInitialization :: Step -> Bool
isInitialization =
    all (flip (<=) 50) . fmap abs . coordinates . region


data Region
    = Region
        { start :: Position
        , end :: Position
        }
    deriving Show


parseRegion :: String -> Region
parseRegion input =
    case List.splitOn "," input of
        [ x, y, z ] ->
            let
                ( xStart, xEnd ) =
                    parseRange x

                ( yStart, yEnd ) =
                    parseRange y

                ( zStart, zEnd ) =
                    parseRange z
            in
            Region (Position xStart yStart zStart) (Position xEnd yEnd zEnd)

        _ ->
            error "invalid region!"
    where
        parseRange range =
            case List.splitOn ".." (drop 2 range) of
                [ start, end ] ->
                    ( read start, read end )

                _ ->
                    error "invalid region!"


coordinates :: Region -> [Int]
coordinates (Region (Position xStart yStart zStart) (Position xEnd yEnd zEnd)) =
    [ xStart, yStart, zStart, xEnd, yEnd, zEnd ]


cubes :: Region -> Set Cube
cubes (Region (Position xStart yStart zStart) (Position xEnd yEnd zEnd)) =
    Set.fromList $
        Cube
            <$> [
                Position x y z
                | x <- range xStart xEnd
                , y <- range yStart yEnd
                , z <- range zStart zEnd
            ]


range :: Int -> Int -> [Int]
range start end =
    enumFromThenTo start (start + 1) end


data Position
    = Position { x :: Int, y :: Int, z :: Int }
    deriving (Eq, Ord, Show)
