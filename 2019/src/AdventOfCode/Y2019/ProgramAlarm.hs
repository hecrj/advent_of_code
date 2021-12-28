module AdventOfCode.Y2019.ProgramAlarm
    ( day
    ) where

import qualified AdventOfCode
import qualified AdventOfCode.Intcode as Intcode
import qualified Data.List as List


day :: AdventOfCode.Day
day =
    AdventOfCode.day "1202 Program Alarm" part1 part2
    where
        part1 =
            assist 12 2 . Intcode.load . Intcode.parse

        part2 input =
            let
                program =
                    Intcode.load (Intcode.parse input)

                ( noun, verb ) =
                    head $
                        List.filter ((==) 19690720 . flip (uncurry assist) program)
                            [
                                ( noun, verb )
                                | noun <- [0..99]
                                , verb <- [0..99]
                            ]
            in
            noun * 100 + verb

        assist noun verb =
            Intcode.value (Intcode.address 0)
                . fst
                    . Intcode.run (Intcode.input [])
                        . Intcode.replace (Intcode.address 2) verb
                            . Intcode.replace (Intcode.address 1) noun
