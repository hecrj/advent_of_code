{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
module AdventOfCode.Y2021.BeaconScanner
    ( day
    ) where

import Data.Set (Set)
import qualified AdventOfCode
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Beacon Scanner" part1 part2
    where
        part1 =
            Set.size . beacons . findScanners . parse

        part2 =
            const 0


parse :: String -> [Report]
parse =
    fmap parseReport . List.splitOn "\n\n"


data Report
    = Report [Position]


parseReport :: String -> Report
parseReport =
    Report . fmap parsePosition . drop 1 . lines


findScanners :: [Report] -> [Scanner]
findScanners [] =
    []
findScanners (Report readings : rest) =
    let
        scanners =
            find' [ (Scanner (Position 0 0 0) readings) ] rest []
    in
    if length scanners == length rest + 1 then
        scanners

    else
        error "not all scanners found!"
    where
        find' (current : next) reports found =
            let
                newFound =
                    fmap (determine current) reports
            in
            find'
                (next ++ Maybe.catMaybes newFound)
                (fmap snd (filter (Maybe.isNothing . fst) (zip newFound reports)))
                (current : found)
        find' pending _ found =
            pending ++ found


beacons :: [Scanner] -> Set Position
beacons =
    Set.fromList . concat . fmap _beacons


data Scanner
    = Scanner
        { _position :: Position
        , _beacons :: [Position]
        }
    deriving Show


determine :: Scanner -> Report -> Maybe Scanner
determine (Scanner _ beacons) (Report readings) =
    let
        rotated =
            List.transpose $ fmap rotations readings
    in
    do
        ( distance, readings' ) <-
            List.find
                (uncurry (overlap beacons))
                (distances beacons rotated)

        Just $ Scanner (sub distance (Position 0 0 0)) (fmap (sub distance) readings')


overlap :: [Position] -> Vector -> [Position] -> Bool
overlap beacons candidate readings =
    let
        intersection =
            List.intersect readings $ fmap (add candidate) beacons
    in
    length intersection >= 12


data Position
    = Position { _x :: Int, _y :: Int, _z :: Int }
    deriving (Eq, Ord, Show)


parsePosition :: String -> Position
parsePosition =
    parseCoordinates . List.splitOn ","
    where
        parseCoordinates [ x, y, z ] =
            Position (read x) (read y) (read z)
        parseCoordinates _ =
            error "invalid position!"


distances :: [Position] -> [[Position]] -> [( Vector, [Position] )]
distances list rotations =
    concat $
        [
            fmap (\candidate -> ( distance a candidate, b )) b
            | a <- list
            , b <- rotations
        ]


rotations :: Position -> [Position]
rotations (Position x y z) =
    [ Position x y z
    , Position x (-z) y
    , Position x (-y) (-z)
    , Position x z (-y)
    , Position (-x) y (-z)
    , Position (-x) z y
    , Position (-x) (-y) z
    , Position (-x) (-z) (-y)
    , Position y (-x) z
    , Position y (-z) (-x)
    , Position y x (-z)
    , Position y z x
    , Position (-y) x z
    , Position (-y) (-z) x
    , Position (-y) (-x) (-z)
    , Position (-y) z (-x)
    , Position z y (-x)
    , Position z x y
    , Position z (-y) x
    , Position z (-x) (-y)
    , Position (-z) y x
    , Position (-z) (-x) y
    , Position (-z) (-y) (-x)
    , Position (-z) x (-y)
    ]


distance :: Position -> Position -> Vector
distance (Position x1 y1 z1) (Position x2 y2 z2) =
    Vector (x2 - x1) (y2 - y1) (z2 - z1)


data Vector
    = Vector { _x :: Int, _y :: Int, _z :: Int }
    deriving Show


add :: Vector -> Position -> Position
add (Vector vx vy vz) (Position x y z) =
    Position (x + vx) (y + vy) (z + vz)


sub :: Vector -> Position -> Position
sub (Vector vx vy vz) (Position x y z) =
    Position (x - vx) (y - vy) (z - vz)
