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
            assist 12 2

        part2 input =
            let
                ( noun, verb ) =
                    head $
                        List.filter ((==) 19690720 . flip (uncurry assist) input)
                            [
                                ( noun, verb )
                                | noun <- [0..99]
                                , verb <- [0..99]
                            ]
            in
            noun * 100 + verb

        assist noun verb =
            value 0 . run . replace 2 verb . replace 1 noun . load . parse


newtype Program
    = Program (Vector Int)
    deriving Show


parse :: String -> Program
parse =
    Program . Vector.fromList . fmap read . List.splitOn ","


data Memory
    = Memory (Vector Int)
    deriving Show


load :: Program -> Memory
load (Program program) =
    Memory program


replace :: Int -> Int -> Memory -> Memory
replace n value (Memory memory) =
    Memory (memory // [ ( n, value ) ])


value :: Int -> Memory -> Int
value n (Memory memory) =
    memory ! n


operate :: (Int -> Int -> Int) -> Int -> Int -> Int -> Memory -> Memory
operate f a b c (Memory memory) =
    Memory (memory // [ ( c, f (memory ! a) (memory ! b) ) ])


run :: Memory -> Memory
run =
    _memory . head . reverse . List.unfoldr run' . Just . flip Computer 0
    where
        run' Nothing =
            Nothing
        run' (Just computer@(Computer memory counter)) =
            let
                ( instruction, counter' ) =
                    peek counter memory

                memory' =
                    evaluate instruction memory
            in
            Just ( computer, flip Computer counter' <$> memory' )


data Computer
    = Computer
        { _memory :: Memory
        , _counter :: Int
        }
    deriving Show


data Instruction
    = Add Int Int Int
    | Mul Int Int Int
    | Halt


peek :: Int -> Memory -> ( Instruction, Int )
peek counter (Memory memory) =
    case memory ! counter of
        1 ->
            ternary Add

        2 ->
            ternary Mul

        99 ->
            ( Halt, counter + 1 )

        _ ->
            error ("unknow opcode: " ++ show opcode)
    where
        opcode =
            memory ! counter

        ternary instruction =
            case Vector.toList $ Vector.slice (counter + 1) 3 memory of
                [ a, b, c ] ->
                    ( instruction a b c, counter + 4 )

                _ ->
                    error ("invalid counter: " ++ show counter)


evaluate :: Instruction -> Memory -> Maybe Memory
evaluate (Add a b c) =
    Just . operate (+) a b c
evaluate (Mul a b c) =
    Just . operate (*) a b c
evaluate Halt =
    const Nothing
