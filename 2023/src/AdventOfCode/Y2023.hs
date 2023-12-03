module AdventOfCode.Y2023
    ( year
    )
where

import qualified AdventOfCode
import qualified AdventOfCode.Y2023.CubeConundrum as CubeConundrum
import qualified AdventOfCode.Y2023.Trebuchet as Trebuchet


year :: AdventOfCode.Year
year =
    AdventOfCode.year
        2023
        [ Trebuchet.day
        , CubeConundrum.day
        ]