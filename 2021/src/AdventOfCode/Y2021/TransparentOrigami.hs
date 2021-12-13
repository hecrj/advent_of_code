module AdventOfCode.Y2021.TransparentOrigami
    ( day
    ) where

import Data.Set (Set)
import qualified AdventOfCode
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Set as Set


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Transparent Origami" part1 part2
    where
        part1 =
            length . dots . (!! 0) . uncurry folds . parse

        part2 =
            length . dots . AdventOfCode.debug . (!! 0) . reverse . uncurry folds . parse


parse :: String -> ( Transparency, [Instruction] )
parse input =
    case List.splitOn "\n\n" input of
        [ transparency, instructions ] ->
            ( parseTransparency transparency
            , fmap parseInstruction (lines instructions)
            )

        _ ->
            error "invalid input!"


data Transparency
    = Transparency
        { dots :: Set Dot
        }


instance Show Transparency where
    show (Transparency dots) =
        let
            width =
                maximum $ fmap x $ Set.toList dots

            height =
                maximum $ fmap y $ Set.toList dots
        in
        concat $ List.intersperse "\n" $ fmap (showRow width) [0..height]
        where
            showRow width y =
                fmap (\x -> if Set.member (Dot x y) dots then '#' else '.') [0..width]


parseTransparency :: String -> Transparency
parseTransparency input =
    let
        dots =
            fmap parseDot $ lines input
    in
    Transparency (Set.fromList dots)


folds :: Transparency -> [Instruction] -> [Transparency]
folds transparency =
    reverse
        . snd
            . foldl
                (\( transparency, transparencies ) instruction ->
                    let
                        new =
                            fold transparency instruction
                    in
                    ( new, new : transparencies )
                )
                ( transparency, [] )


fold :: Transparency -> Instruction -> Transparency
fold (Transparency dots) (FoldUp axis) =
    Transparency $ Set.map (foldUp axis) dots
    where
        foldUp axis (Dot x y)
            | y > axis =
                Dot x (axis - (y - axis))
            | otherwise =
                Dot x y
fold (Transparency dots) (FoldLeft axis) =
    Transparency $ Set.map (foldLeft axis) dots
    where
        foldLeft axis (Dot x y)
            | x > axis =
                Dot (axis - (x - axis)) y
            | otherwise =
                Dot x y


data Dot
    = Dot { x :: Int, y :: Int }
    deriving (Ord, Eq, Show)


parseDot :: String -> Dot
parseDot input =
    case List.splitOn "," input of
        [ x, y ] ->
            Dot (read x) (read y)

        _ ->
            error "invalid dot!"


data Instruction
    = FoldUp Int
    | FoldLeft Int


parseInstruction :: String -> Instruction
parseInstruction input =
    case List.splitOn "fold along " input of
        [ _, axis ] ->
            case List.splitOn "=" axis of
                [ "x", x ] ->
                    FoldLeft (read x)

                [ "y", y ] ->
                    FoldUp (read y)

                _ ->
                    error "invalid instruction!"

        _ ->
            error "invalid instruction!"
