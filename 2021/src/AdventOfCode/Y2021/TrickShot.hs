module AdventOfCode.Y2021.TrickShot
    ( day
    ) where

import qualified AdventOfCode
import qualified Data.List.Split as List


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Trick Shot" part1 part2
    where
        part1 =
            maximum . fmap maxHeight . shotsOnTarget . parseTarget

        part2 =
            length . shotsOnTarget . parseTarget


data Target
    = Target { x :: ( Int, Int ), y :: ( Int, Int ) }


parseTarget :: String -> Target
parseTarget input =
    case List.splitOn ", " (drop (length "target area: ") input) of
        [ xRange, yRange ] ->
            case
                ( List.splitOn ".." (drop (length "x=") xRange)
                , List.splitOn ".." (drop (length "y=") yRange)
                )
            of
                ( [ xStart, xEnd ], [ yStart, yEnd ] ) ->
                    Target ( read xStart, read xEnd ) ( read yStart, read yEnd )

                _ ->
                    error "invalid target!"

        _ ->
            error "invalid target!"


newtype Velocity
    = Velocity ( Int, Int )
    deriving Show


shotsOnTarget :: Target -> [Velocity]
shotsOnTarget target@(Target ( _, xEnd ) ( yStart, _ )) =
    let
        boundary =
            max xEnd (abs yStart)
    in
    filter (hitsTarget target) $
        [
            Velocity ( x, y )
            | x <- [0..boundary]
            , y <- enumFromThenTo (-boundary) (-boundary + 1) boundary
        ]


hitsTarget :: Target -> Velocity -> Bool
hitsTarget target =
    simulate target ( 0, 0 )
    where
        simulate target@(Target ( xStart, xEnd ) ( yStart, yEnd )) ( x, y ) (Velocity ( xVelocity, yVelocity ))
            | x >= xStart && x <= xEnd && y >= yStart && y <= yEnd =
                True
            | x > xEnd || y < yStart =
                False
            | otherwise =
                simulate target
                    ( x + xVelocity, y + yVelocity )
                    (Velocity ( xVelocity - signum xVelocity, yVelocity - 1 ))


maxHeight :: Velocity -> Int
maxHeight (Velocity ( x, y ))
    | y > 0 =
        y + maxHeight (Velocity ( x, y - 1 ))
    | otherwise =
        0
