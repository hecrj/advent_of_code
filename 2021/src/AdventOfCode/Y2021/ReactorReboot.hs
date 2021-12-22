module AdventOfCode.Y2021.ReactorReboot
    ( day
    ) where

import Data.Set (Set)
import qualified AdventOfCode
import qualified Data.List.Split as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Reactor Reboot" part1 part2
    where
        part1 =
            count . initialize . parse

        part2 =
            count . run . parse


parse :: String -> [Step]
parse =
    fmap parseStep . lines


data Reactor
    = Reactor (Set Region)
    deriving Show


initialize :: [Step] -> Reactor
initialize =
    run . filter isInitialization . reverse . drop 2 . reverse


run :: [Step] -> Reactor
run =
    foldr apply (Reactor Set.empty) . reverse


apply :: Step -> Reactor -> Reactor
apply (On region) (Reactor enabled) =
    case AdventOfCode.head (collisions region (Set.toList enabled)) of
        Just collision ->
            foldr apply (Reactor enabled) $ fmap On (split collision region)

        Nothing ->
            Reactor (Set.insert region enabled)
apply (Off region) (Reactor enabled) =
    Reactor $ Set.fromList $ concat $ remove region <$> Set.toList enabled


count :: Reactor -> Int
count (Reactor regions) =
    sum $ cubes <$> Set.toList regions


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


isInitialization :: Step -> Bool
isInitialization =
    all (flip (<=) 50) . fmap abs . coordinates . region


region :: Step -> Region
region (On region) =
    region
region (Off region) =
    region


data Region
    = Region
        { start :: Position
        , end :: Position
        }
    deriving (Eq, Ord, Show)


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
            Region (Position xStart yStart zStart) (Position (xEnd + 1) (yEnd + 1) (zEnd + 1))

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
    [ xStart, yStart, zStart, xEnd - 1, yEnd - 1, zEnd - 1 ]


cubes :: Region -> Int
cubes (Region (Position xStart yStart zStart) (Position xEnd yEnd zEnd)) =
    let
        x =
            max 0 (xEnd - xStart)

        y =
            max 0 (yEnd - yStart)

        z =
            max 0 (zEnd - zStart)
    in
    x * y * z


collisions :: Region -> [Region] -> [Region]
collisions region =
    Maybe.catMaybes . fmap (collision region)


collision :: Region -> Region -> Maybe Region
collision (Region startR endR) (Region startT endT) =
    let
        candidate =
            Region (maximize startR startT) (minimize endR endT)
    in
    if cubes candidate > 0 then
        Just candidate

    else
        Nothing


remove :: Region -> Region -> [Region]
remove region target =
    case collision region target of
        Just collision ->
            split collision target

        Nothing ->
            [ target ]


split :: Region -> Region -> [Region]
split (Region startR endR) (Region startT endT) =
    filter ((/=) 0 . cubes)
        [ Region
            startT
            (Position (x endT) (y endT) (z startR))
        , Region
            (Position (x startT) (y startT) (z startR))
            (Position (x startR) (y endT) (z endR))
        , Region
            (Position (x endR) (y startT) (z startR))
            (Position (x endT) (y endT) (z endR))
        , Region
            (Position (x startR) (y startT) (z startR))
            (Position (x endR) (y startR) (z endR))
        , Region
            (Position (x startR) (y endR) (z startR))
            (Position (x endR) (y endT) (z endR))
        , Region
            (Position (x startT) (y startT) (z endR))
            endT
        ]


data Position
    = Position { x :: Int, y :: Int, z :: Int }
    deriving (Eq, Ord, Show)


maximize :: Position -> Position -> Position
maximize (Position x1 y1 z1) (Position x2 y2 z2) =
    Position (max x1 x2) (max y1 y2) (max z1 z2)


minimize :: Position -> Position -> Position
minimize (Position x1 y1 z1) (Position x2 y2 z2) =
    Position (min x1 x2) (min y1 y2) (min z1 z2)
