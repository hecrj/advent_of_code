module AdventOfCode.Y2019.AmplificationCircuit
    ( day
    ) where

import qualified AdventOfCode
import qualified AdventOfCode.Intcode as Intcode
import qualified Data.List as List


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Amplification Circuit" part1 part2
    where
        part1 =
            maximum . runAll (List.permutations [0..4]) . Intcode.parse

        part2 =
            const 0

        runAll permutations program =
            fmap (run program) permutations


run :: Intcode.Program -> [Int] -> Int
run program =
    foldr runAmplifier 0
    where
        runAmplifier phase input =
            let
                ( _memory, output ) =
                    Intcode.run (Intcode.input [ phase, input ]) (Intcode.load program)
            in
            head output
