module AdventOfCode.Y2021.TrenchMap
    ( day
    ) where

import Data.Vector (Vector, (!))
import qualified AdventOfCode
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Maybe as Maybe
import qualified Data.Vector as Vector


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Trench Map" part1 part2
    where
        part1 =
            Maybe.fromMaybe (error "infinite lit pixels!")
                . count Light
                    . flip (!!) 2
                        . uncurry steps
                            . parse

        part2 =
            const 0


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


instance Show Pixel where
    show Dark =
        "."
    show Light =
        "#"


data Image
    = Image
        { _background :: Pixel
        , _pixels :: [[Pixel]]
        }


instance Show Image where
    show =
        concat . List.intersperse "\n" . fmap (concat . fmap show) . _pixels . pad


parseImage :: String -> Image
parseImage =
    Image Dark . fmap (fmap parsePixel) . lines


count :: Pixel -> Image -> Maybe Int
count p (Image background pixels)
    | p == background =
        Nothing
    | otherwise =
        Just $ length $ filter ((==) p) (concat pixels)


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
apply (Algorithm algorithm) (Image background pixels) =
    let
        enumerated =
            zip [0..] $ fmap (fmap fst . zip [0..]) pixels
    in
    Image background' $ fmap (uncurry enhanceRow) enumerated
    where
        background' =
            case background of
                Dark ->
                    algorithm ! 0

                Light ->
                    algorithm ! 511

        enhanceRow i row =
            fmap (enhancePixel i) row

        enhancePixel i j =
            let
                scope' =
                    scope i j pixels
            in
            if length scope' == 9 then
                algorithm ! toDecimal scope'

            else
                background'


scope :: Int -> Int -> [[Pixel]] -> [Pixel]
scope i j pixels =
    [
        pixels !! ni !! nj
        | ni <- [i - 1..i + 1]
        , nj <- [j - 1..j + 1]
        , 0 <= ni
            && ni < length pixels
                && 0 <= nj
                    && nj < length (List.transpose pixels)
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
pad (Image background pixels) =
    Image background $ List.transpose $ pad' $ List.transpose $ pad' pixels
    where
        pad' [] =
            []
        pad' pixels@(row : _) =
            let
                padding =
                    replicate (length row) background
            in
            padding : pixels ++ [ padding ]
