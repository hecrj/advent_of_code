module AdventOfCode.Y2023.IfYouGiveASeedAFertilizer (day) where

import qualified AdventOfCode
import qualified Data.List.Split as List

import Control.Arrow ((>>>))
import Data.Maybe (mapMaybe)
import Prelude hiding (lookup)


day :: AdventOfCode.Day
day = AdventOfCode.day "If You Give A Seed A Fertilizer" part1 part2


part1 :: AdventOfCode.Solution
part1 =
    fromIntegral . minimum . locations . parse


part2 :: AdventOfCode.Solution
part2 = const 0


data Almanac = Almanac
    { _seeds :: [Seed]
    , _soil :: Map Seed Soil
    , _fertilizer :: Map Soil Fertilizer
    , _water :: Map Fertilizer Water
    , _light :: Map Water Light
    , _temperature :: Map Light Temperature
    , _humidity :: Map Temperature Humidity
    , _location :: Map Humidity Location
    }
    deriving (Show)


parse :: String -> Almanac
parse input =
    let
        [ seeds
            , soil
            , fertilizer
            , water
            , light
            , temperature
            , humidity
            , location
            ] = List.splitOn "\n\n" input
    in
        Almanac
            (read <$> tail (words seeds))
            (parseMap soil)
            (parseMap fertilizer)
            (parseMap water)
            (parseMap light)
            (parseMap temperature)
            (parseMap humidity)
            (parseMap location)


locations :: Almanac -> [Location]
locations almanac =
    locate almanac <$> _seeds almanac


locate :: Almanac -> Seed -> Location
locate (Almanac _ soil fertilizer water light temperature humidity location) =
    lookup soil
        >>> lookup fertilizer
        >>> lookup water
        >>> lookup light
        >>> lookup temperature
        >>> lookup humidity
        >>> lookup location


newtype Map key value = Map [Range key value] deriving (Show)


parseMap :: (Integral key, Integral value, Read key, Read value) => String -> Map key value
parseMap =
    Map . fmap parseRange . tail . lines


lookup :: (Integral key, Integral value) => Map key value -> key -> value
lookup (Map ranges) key =
    let
        matches = mapMaybe (match key) ranges
    in
        case matches of
            (match : _) -> match
            [] -> fromIntegral key


data Range key value = Range
    { _destinationStart :: key
    , _sourceStart :: value
    , _length :: Int
    }
    deriving (Show)


parseRange :: (Integral key, Integral value, Read key, Read value) => String -> Range key value
parseRange range =
    let
        [destinationStart, sourceStart, length] = words range
    in
        Range (read destinationStart) (read sourceStart) (read length)


match :: (Integral key, Integral value) => key -> Range key value -> Maybe value
match key (Range destinationStart sourceStart length)
    | sourceStart <= fromIntegral key && key < fromIntegral sourceStart + fromIntegral length =
        Just $ fromIntegral $ destinationStart + (key - fromIntegral sourceStart)
    | otherwise = Nothing


newtype Seed = Seed Int
    deriving (Eq, Ord, Num, Enum, Integral, Real, Show)
    deriving newtype (Read)


newtype Soil = Soil Int
    deriving (Eq, Ord, Num, Enum, Integral, Real, Show)
    deriving newtype (Read)


newtype Fertilizer = Fertilizer Int
    deriving (Eq, Ord, Num, Enum, Integral, Real, Show)
    deriving newtype (Read)


newtype Water = Water Int
    deriving (Eq, Ord, Num, Enum, Integral, Real, Show)
    deriving newtype (Read)


newtype Light = Light Int
    deriving (Eq, Ord, Num, Enum, Integral, Real, Show)
    deriving newtype (Read)


newtype Temperature = Temperature Int
    deriving (Eq, Ord, Num, Enum, Integral, Real, Show)
    deriving newtype (Read)


newtype Humidity = Humidity Int
    deriving (Eq, Ord, Num, Enum, Integral, Real, Show)
    deriving newtype (Read)


newtype Location = Location Int
    deriving (Eq, Ord, Num, Enum, Integral, Real, Show)
    deriving newtype (Read)
