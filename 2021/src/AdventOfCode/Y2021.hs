module AdventOfCode.Y2021
    ( year
    ) where

import qualified AdventOfCode
import qualified AdventOfCode.Y2021.Amphipod as Amphipod
import qualified AdventOfCode.Y2021.ArithmeticLogicUnit as ArithmeticLogicUnit
import qualified AdventOfCode.Y2021.BeaconScanner as BeaconScanner
import qualified AdventOfCode.Y2021.BinaryDiagnostic as BinaryDiagnostic
import qualified AdventOfCode.Y2021.Chiton as Chiton
import qualified AdventOfCode.Y2021.DiracDice as DiracDice
import qualified AdventOfCode.Y2021.Dive as Dive
import qualified AdventOfCode.Y2021.DumboOctopus as DumboOctopus
import qualified AdventOfCode.Y2021.ExtendedPolymerization as ExtendedPolymerization
import qualified AdventOfCode.Y2021.GiantSquid as GiantSquid
import qualified AdventOfCode.Y2021.HydrothermalVenture as HydrothermalVenture
import qualified AdventOfCode.Y2021.Lanternfish as Lanternfish
import qualified AdventOfCode.Y2021.PacketDecoder as PacketDecoder
import qualified AdventOfCode.Y2021.PassagePathing as PassagePathing
import qualified AdventOfCode.Y2021.ReactorReboot as ReactorReboot
import qualified AdventOfCode.Y2021.SeaCucumber as SeaCucumber
import qualified AdventOfCode.Y2021.SevenSegmentSearch as SevenSegmentSearch
import qualified AdventOfCode.Y2021.SmokeBasin as SmokeBasin
import qualified AdventOfCode.Y2021.Snailfish as Snailfish
import qualified AdventOfCode.Y2021.SonarSweep as SonarSweep
import qualified AdventOfCode.Y2021.SyntaxScoring as SyntaxScoring
import qualified AdventOfCode.Y2021.TheTreacheryOfWhales as TheTreacheryOfWhales
import qualified AdventOfCode.Y2021.TransparentOrigami as TransparentOrigami
import qualified AdventOfCode.Y2021.TrenchMap as TrenchMap
import qualified AdventOfCode.Y2021.TrickShot as TrickShot


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
        , TransparentOrigami.day
        , ExtendedPolymerization.day
        , Chiton.day
        , PacketDecoder.day
        , TrickShot.day
        , Snailfish.day
        , BeaconScanner.day
        , TrenchMap.day
        , DiracDice.day
        , ReactorReboot.day
        , Amphipod.day
        , ArithmeticLogicUnit.day
        , SeaCucumber.day
        ]
