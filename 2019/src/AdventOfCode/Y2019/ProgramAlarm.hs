module AdventOfCode.Y2019.ProgramAlarm
    ( day
    ) where

import Data.Vector (Vector, (//), (!))
import qualified AdventOfCode
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Vector as Vector


day :: AdventOfCode.Day
day =
    AdventOfCode.day "1202 Program Alarm" part1 part2
    where
        part1 =
            value 0 . run . replace 2 2 . replace 1 12 . parse

        part2 =
            const 0


newtype Program
    = Program (Vector Int)
    deriving Show


parse :: String -> Program
parse =
    Program . Vector.fromList . fmap read . List.splitOn ","


replace :: Int -> Int -> Program -> Program
replace n value (Program program) =
    Program (program // [ ( n, value ) ])


value :: Int -> Program -> Int
value n (Program program) =
    program ! n


operate :: (Int -> Int -> Int) -> Int -> Int -> Int -> Program -> Program
operate f a b c (Program program) =
    Program (program // [ ( c, f (program ! a) (program ! b) ) ])


run :: Program -> Program
run =
    _program . head . reverse . List.unfoldr run' . Just . flip Computer 0
    where
        run' Nothing =
            Nothing
        run' (Just computer@(Computer program counter)) =
            let
                ( instruction, counter' ) =
                    peek counter program

                program' =
                    evaluate instruction program
            in
            Just ( computer, flip Computer counter' <$> program' )


data Computer
    = Computer
        { _program :: Program
        , _counter :: Int
        }
    deriving Show


data Instruction
    = Add Int Int Int
    | Mul Int Int Int
    | Halt


peek :: Int -> Program -> ( Instruction, Int )
peek counter (Program program) =
    case program ! counter of
        1 ->
            ternary Add

        2 ->
            ternary Mul

        99 ->
            ( Halt, counter )

        _ ->
            error ("unknow opcode: " ++ show opcode)
    where
        opcode =
            program ! counter

        ternary instruction =
            case Vector.toList $ Vector.slice (counter + 1) 3 program of
                [ a, b, c ] ->
                    ( instruction a b c, counter + 4 )

                _ ->
                    error ("invalid counter: " ++ show counter)


evaluate :: Instruction -> Program -> Maybe Program
evaluate (Add a b c) =
    Just . operate (+) a b c
evaluate (Mul a b c) =
    Just . operate (*) a b c
evaluate Halt =
    const Nothing
