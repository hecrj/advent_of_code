module AdventOfCode.Y2019.SunnyWithAChanceOfAsteroids
    ( day
    ) where

import qualified AdventOfCode
import qualified AdventOfCode.Intcode as Intcode


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Sunny with a Chance of Asteroids" part1 part2
    where
        part1 =
            head
                . snd
                    . Intcode.run (Intcode.input [ 1 ])
                        . Intcode.load
                            . Intcode.parse

        part2 =
            const 0
