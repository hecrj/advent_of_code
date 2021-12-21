module AdventOfCode.Y2021.TrenchMap
    ( day
    ) where

import Data.Set (Set)
import Data.Vector (Vector, (!))
import qualified AdventOfCode
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Trench Map" part1 part2
    where
        part1 =
            countLight . enhanceN 2 . parse

        part2 =
            countLight . enhanceN 50 . parse

        enhanceN n =
            flip (!!) n . uncurry steps

        countLight =
            Maybe.fromMaybe (error "infinite lit pixels!") . count Light


parse :: String -> ( Algorithm, Image )
parse input =
    case List.splitOn "\n\n" input of
        [ algorithm, image ] ->
            ( parseAlgorithm algorithm, parseImage image )

        _ ->
            error "invalid input!"


data Pixel
    = Dark
    | Light
    deriving Eq


parsePixel :: Char -> Pixel
parsePixel '.' =
    Dark
parsePixel '#' =
    Light
parsePixel _ =
    error "invalid pixel!"


toggle :: Pixel -> Pixel
toggle Light =
    Dark
toggle Dark =
    Light


instance Show Pixel where
    show Dark =
        "."
    show Light =
        "#"


data Image
    = Image
        { _background :: Pixel
        , _width :: Int
        , _height :: Int
        , _pixels :: Set Position
        }


data Position
    = Position
        { _i :: Int
        , _j :: Int
        }
    deriving (Eq, Ord)


instance Show Image where
    show =
        concat . List.intersperse "\n" . fmap (concat . fmap show) . grid . pad


parseImage :: String -> Image
parseImage input =
    let
        grid =
            fmap (fmap parsePixel) $ lines input

        width =
            length grid

        height =
            length $ List.transpose grid

        pixels =
            Set.fromList $
                filter (\(Position i j) -> grid !! i !! j == Light) $
                    concat (positions width height)
    in
    Image Dark width height pixels


grid :: Image -> [[Pixel]]
grid (Image background width height pixels) =
    fmap (fmap (toPixel . flip Set.member pixels)) (positions width height)
    where
        toPixel False =
            background
        toPixel True =
            toggle background


positions :: Int -> Int -> [[Position]]
positions width height =
    fmap (\( i, row ) -> fmap (Position i) row) $
        zip [0..height - 1] (repeat [0..width - 1])


count :: Pixel -> Image -> Maybe Int
count p (Image background _ _ pixels)
    | p == background =
        Nothing
    | otherwise =
        Just $ Set.size pixels


data Algorithm
    = Algorithm (Vector Pixel)


parseAlgorithm :: String -> Algorithm
parseAlgorithm =
    Algorithm . Vector.fromList . fmap parsePixel . concat . lines


steps :: Algorithm -> Image -> [Image]
steps =
    iterate . enhance


enhance :: Algorithm -> Image -> Image
enhance algorithm =
    apply algorithm . pad . pad


apply :: Algorithm -> Image -> Image
apply (Algorithm algorithm) image@(Image background width height _) =
    Image background' width height enhancedPixels
    where
        enhancedPixels =
            Set.fromList $ filter enhancePixel $ concat (positions width height)

        background' =
            case background of
                Dark ->
                    algorithm ! 0

                Light ->
                    algorithm ! 511

        enhancePixel position =
            let
                scope' =
                    scope image position

                pixel =
                    if length scope' == 9 then
                        algorithm ! toDecimal scope'

                    else
                        background'
            in
            pixel /= background'


scope :: Image -> Position -> [Pixel]
scope (Image background width height pixels) (Position i j) =
    [
        if Set.member (Position ni nj) pixels then
            toggle background

        else
            background
        | ni <- [i - 1..i + 1]
        , nj <- [j - 1..j + 1]
        , 0 <= ni && ni < height && 0 <= nj && nj < width
    ]


toDecimal :: [Pixel] -> Int
toDecimal =
    sum
        . fmap (uncurry (*))
            . zip powersOf2
                . reverse
                    . fmap bit
    where
        powersOf2 =
            fmap ((^) 2) [0..]

        bit Dark =
            0
        bit Light =
            1


pad :: Image -> Image
pad (Image background width height pixels) =
    let
        translation =
            Set.map (\(Position i j) -> Position (i + 1) (j + 1)) pixels
    in
    Image background (width + 2) (height + 2) translation
