module AdventOfCode.Y2019.TheTyrannyOfTheRocketEquation
    ( day
    ) where

import qualified AdventOfCode
import qualified Data.List as List


day :: AdventOfCode.Day
day =
    AdventOfCode.day "The Tyranny of the Rocket Equation" part1 part2
    where
        part1 =
            sum . fmap fuel . parse

        part2 =
            sum . fmap properFuel . parse


parse :: String -> [Module]
parse =
    fmap parseModule . lines


data Module
    = Module { _mass :: Int }


parseModule :: String -> Module
parseModule =
    Module . read


fuel :: Module -> Int
fuel (Module mass) =
    mass `div` 3 - 2


properFuel :: Module -> Int
properFuel =
    sum
        . List.unfoldr
            (\fuel_ ->
                if fuel_ > 0 then
                    Just ( fuel_, fuel (Module fuel_) )

                else
                    Nothing
            )
            . fuel
