module AdventOfCode.Y2019.SensorBoost
    ( day
    ) where

import qualified AdventOfCode
import qualified AdventOfCode.Intcode as Intcode


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Sensor Boost" part1 part2
    where
        part1 =
            run 1

        part2 =
            run 2

        run n =
            head . snd . Intcode.run (Intcode.input [ n ]) . Intcode.load . Intcode.parse
