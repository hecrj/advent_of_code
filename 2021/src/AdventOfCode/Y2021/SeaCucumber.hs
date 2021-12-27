module AdventOfCode.Y2021.SeaCucumber
    ( day
    ) where

import Data.Map.Strict (Map)
import qualified AdventOfCode
import qualified Data.List as List
import qualified Data.Map.Strict as Map


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Sea Cucumber" part1 part2
    where
        part1 =
            (+ 1) . length . moves . parse

        part2 =
            const 0


data Floor
    = Floor
        { _width :: Int
        , _height :: Int
        , _cucumbers :: Map Position Cucumber
        }
    deriving Eq


parse :: String -> Floor
parse input =
    let
        width =
            length $ List.transpose $ lines input

        height =
            length $ lines input
    in
    Floor width height (gridWith parseCucumber input)


moves :: Floor -> [Floor]
moves =
    List.unfoldr
        (\floor ->
            let
                newFloor =
                    move floor
            in
            if floor == newFloor then
                Nothing

            else
                Just ( newFloor, newFloor )
        )


move :: Floor -> Floor
move =
    move' South . move' East
    where
        move' kind (Floor width height cucumbers) =
            Floor width height cucumbers'
            where
                cucumbers' =
                    Map.fromList $ fmap (uncurry maybeMove) $ Map.toList cucumbers

                maybeMove position cucumber
                    | kind == cucumber =
                        let
                            target =
                                forward width height cucumber position
                        in
                        if Map.member target cucumbers then
                            ( position, cucumber )

                        else
                            ( target, cucumber )
                maybeMove position cucumber =
                    ( position, cucumber )


data Cucumber
    = East
    | South
    deriving (Eq, Show)


parseCucumber :: Char -> Maybe Cucumber
parseCucumber '>' =
    Just East
parseCucumber 'v' =
    Just South
parseCucumber _ =
    Nothing


data Position
    = Position Int Int
    deriving (Eq, Ord, Show)


forward :: Int -> Int -> Cucumber -> Position -> Position
forward width _ East (Position i j) =
    Position i ((j + 1) `mod` width)
forward _ height South (Position i j) =
    Position ((i + 1) `mod` height) j


gridWith :: (Char -> Maybe a) -> String -> Map Position a
gridWith parse =
    foldr parseAndInsert Map.empty . cells
    where
        cells =
            concat
                . fmap (\( i, row ) -> fmap (\( j, c ) -> ( Position i j, c )) (zip [0..] row))
                    . zip [0..]
                        . lines

        parseAndInsert ( position, cell ) map =
            case parse cell of
                Just value ->
                    Map.insert position value map

                Nothing ->
                    map


instance Show Floor where
    show (Floor width height cucumbers) =
        concat
            [ "\n"
            , concat $
                List.intersperse "\n" $
                    fmap (uncurry showRow) $
                        zip [0..height - 1] (replicate height [0..width - 1])
            , "\n"
            ]
        where
            showRow i =
                fmap (showCell i)

            showCell i j =
                case Map.lookup (Position i j) cucumbers of
                    Just East ->
                        '>'

                    Just South ->
                        'v'

                    Nothing ->
                        '.'
