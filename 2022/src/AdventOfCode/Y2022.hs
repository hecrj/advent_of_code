module AdventOfCode.Y2022
    ( year
    ) where

import qualified AdventOfCode
import qualified AdventOfCode.Y2022.NoSpaceLeftOnDevice as NoSpaceLeftOnDevice
import qualified AdventOfCode.Y2022.TreetopTreeHouse as TreetopTreeHouse


year :: AdventOfCode.Year
year =
    AdventOfCode.year 2022
        [ AdventOfCode.todo
        , AdventOfCode.todo
        , AdventOfCode.todo
        , AdventOfCode.todo
        , AdventOfCode.todo
        , AdventOfCode.todo
        , NoSpaceLeftOnDevice.day
        , TreetopTreeHouse.day
        ]
