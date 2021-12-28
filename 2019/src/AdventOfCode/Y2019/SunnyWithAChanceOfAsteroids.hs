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
            run 1

        part2 =
            run 5

        run n =
            head
                . snd
                    . Intcode.run (Intcode.input [ n ])
                        . Intcode.load
                            . Intcode.parse
