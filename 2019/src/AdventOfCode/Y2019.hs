module AdventOfCode.Y2019
    ( year
    ) where

import qualified AdventOfCode
import qualified AdventOfCode.Y2019.ProgramAlarm as ProgramAlarm
import qualified AdventOfCode.Y2019.SunnyWithAChanceOfAsteroids as SunnyWithAChanceOfAsteroids
import qualified AdventOfCode.Y2019.TheTyrannyOfTheRocketEquation as TheTyrannyOfTheRocketEquation


year :: AdventOfCode.Year
year =
    AdventOfCode.year 2019
        [ TheTyrannyOfTheRocketEquation.day
        , ProgramAlarm.day
        , AdventOfCode.todo
        , AdventOfCode.todo
        , SunnyWithAChanceOfAsteroids.day
        ]
