module AdventOfCode.Y2020
    ( year
    ) where

import qualified AdventOfCode
import qualified AdventOfCode.Y2020.PasswordPhilosophy as PasswordPhilosophy
import qualified AdventOfCode.Y2020.ReportRepair as ReportRepair


year :: AdventOfCode.Year
year =
    AdventOfCode.year 2020
        [ ReportRepair.day
        , PasswordPhilosophy.day
        ]
