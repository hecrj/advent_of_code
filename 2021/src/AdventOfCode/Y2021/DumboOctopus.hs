module AdventOfCode.Y2021.DumboOctopus
    ( day
    ) where

import Data.Set (Set)
import qualified AdventOfCode
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Dumbo Octopus" flashesAfter100 syncFlashIndex
    where
        flashesAfter100 =
            sum . flashes 100 . parseGrid

        syncFlashIndex =
            Maybe.fromJust . List.findIndex ((==) 100 . snd) . simulate . parseGrid


data Octopus
    = Octopus { energy :: Int }


parseOctopus :: Char -> Octopus
parseOctopus =
    Octopus . read . pure


increase :: Octopus -> Octopus
increase (Octopus energy) =
    Octopus (energy + 1)


willFlash :: Octopus -> Bool
willFlash (Octopus energy) =
    energy > 9


data Grid
    = Grid [[Octopus]]


instance Show Grid where
    show (Grid grid) =
        concat $ List.intersperse "\n" $ fmap (concat . fmap (show . energy)) grid


data Position
    = Position { i :: Int, j :: Int }
    deriving (Eq, Ord, Show)


parseGrid :: String -> Grid
parseGrid =
    Grid . fmap (map parseOctopus) . lines


withPositions :: [[Octopus]] -> [[( Position, Octopus )]]
withPositions =
    fmap (\( i, row ) -> fmap (\( j, octopus ) -> ( Position i j, octopus )) row)
        . zip [0..] . fmap (zip [0..])


simulate :: Grid -> [( Grid, Int )]
simulate grid =
    iterate (flash . fst) ( grid, 0 )


flashes :: Int -> Grid -> [Int]
flashes steps grid =
    map snd $ take (steps + 1) $ simulate grid


flash :: Grid -> ( Grid, Int )
flash (Grid grid) =
    let
        energyIncreased =
            Grid $ map (map increase) grid

        ( flashedGrid, flashedOctopuses ) =
            propagate Set.empty energyIncreased

        finalGrid =
            resetAll flashedOctopuses flashedGrid
    in
    ( Grid finalGrid, Set.size flashedOctopuses )
    where
        resetAll positions (Grid grid) =
            map
                (map
                    (\( p, octopus ) ->
                        if Set.member p positions then
                            Octopus 0

                        else
                            octopus
                    )
                )
                (withPositions grid)


propagate :: Set Position -> Grid -> ( Grid, Set Position )
propagate flashed (Grid grid) =
    let
        flashing =
            Set.fromList $
                map fst $
                    filter (not . (flip Set.member) flashed . fst) $
                        filter (willFlash . snd) $
                            concat $
                                withPositions grid
    in
    if Set.null flashing then
        ( Grid grid, flashed )

    else
        let
            affected =
                concat $ fmap neighbors $ Set.toList flashing

            newGrid =
                increaseAll affected grid
        in
        propagate (Set.union flashed flashing) (Grid newGrid)
    where
        increaseAll [] grid =
            grid
        increaseAll (position : rest) grid =
            increaseAll rest $
                fmap
                    (fmap
                        (\( p, octopus ) ->
                            if p == position then
                                increase octopus

                            else
                                octopus
                        )
                    )
                    (withPositions grid)


neighbors :: Position -> [Position]
neighbors (Position i j) =
    [
        Position ni nj
        | ni <- [i - 1..i + 1]
        , nj <- [j - 1..j + 1]
        , ni /= i || nj /= j
    ]
