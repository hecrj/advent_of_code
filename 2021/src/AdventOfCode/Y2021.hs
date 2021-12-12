module AdventOfCode.Y2021
    ( year
    ) where

import qualified AdventOfCode
import qualified AdventOfCode.Y2021.BinaryDiagnostic as BinaryDiagnostic
import qualified AdventOfCode.Y2021.Dive as Dive
import qualified AdventOfCode.Y2021.DumboOctopus as DumboOctopus
import qualified AdventOfCode.Y2021.GiantSquid as GiantSquid
import qualified AdventOfCode.Y2021.HydrothermalVenture as HydrothermalVenture
import qualified AdventOfCode.Y2021.Lanternfish as Lanternfish
import qualified AdventOfCode.Y2021.PassagePathing as PassagePathing
import qualified AdventOfCode.Y2021.SevenSegmentSearch as SevenSegmentSearch
import qualified AdventOfCode.Y2021.SmokeBasin as SmokeBasin
import qualified AdventOfCode.Y2021.SonarSweep as SonarSweep
import qualified AdventOfCode.Y2021.SyntaxScoring as SyntaxScoring
import qualified AdventOfCode.Y2021.TheTreacheryOfWhales as TheTreacheryOfWhales


year :: AdventOfCode.Year
year =
    AdventOfCode.year 2021
        [ SonarSweep.day
        , Dive.day
        , BinaryDiagnostic.day
        , GiantSquid.day
        , HydrothermalVenture.day
        , Lanternfish.day
        , TheTreacheryOfWhales.day
        , SevenSegmentSearch.day
        , SmokeBasin.day
        , SyntaxScoring.day
        , DumboOctopus.day
        , PassagePathing.day
        ]
