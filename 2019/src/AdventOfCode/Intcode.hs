module AdventOfCode.Intcode
    ( Program
    , Memory
    , Input
    , Address
    , address
    , input
    , parse
    , load
    , replace
    , value
    , run
    ) where

import Data.Vector (Vector, (//), (!))
import Prelude hiding (print)
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Vector as Vector


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


replace :: Address -> Int -> Memory -> Memory
replace (Address n) value (Memory memory) =
    Memory (memory // [ ( n, value ) ])


value :: Address -> Memory -> Int
value (Address n) (Memory memory) =
    memory ! n


run :: Input -> Memory -> ( Memory, [Int] )
run input memory =
    let
        Computer memory' _ (Output output) _ =
            head $ reverse $ List.unfoldr run' $ Just $ initialize memory input
    in
    ( memory', output )
    where
        run' Nothing =
            Nothing
        run' (Just computer@(Computer memory _ _ counter)) =
            let
                instruction =
                    peek counter memory

                computer' =
                    evaluate instruction computer
            in
            Just ( computer, computer' )


data Computer
    = Computer
        { _memory :: Memory
        , _input :: Input
        , _output :: Output
        , _counter :: Address
        }
    deriving Show


initialize :: Memory -> Input -> Computer
initialize memory input =
    Computer memory input (Output []) (Address 0)


write :: (Memory -> Memory) -> Computer -> Computer
write f (Computer memory input output counter) =
    Computer (f memory) input output counter


advance :: Int -> Computer -> Computer
advance n (Computer memory input output (Address counter)) =
    Computer memory input output (Address (counter + n))


read_ :: Computer -> ( Int, Computer )
read_ (Computer memory (Input input) output counter) =
    ( head input, Computer memory (Input (tail input)) output counter )


print :: Int -> Computer -> Computer
print value (Computer memory input (Output output) counter) =
    Computer memory input (Output (value : output)) counter


data Input
    = Input [Int]
    deriving Show


input :: [Int] -> Input
input =
    Input


data Output
    = Output [Int]
    deriving Show


data Instruction
    = Add Parameter Parameter Address
    | Mul Parameter Parameter Address
    | Input_ Address
    | Output_ Parameter
    | Halt
    deriving Show


data Parameter
    = Position Address
    | Immediate Int
    deriving Show


parseParameter :: Int -> Int -> Parameter
parseParameter 0 =
    Position . Address
parseParameter 1 =
    Immediate
parseParameter n =
    error ("invalid parameter mode: " ++ show n)


resolve :: Parameter -> Memory -> Int
resolve (Position address) =
    value address
resolve (Immediate value) =
    const value


data Address
    = Address Int
    deriving Show


address :: Int -> Address
address =
    Address


peek :: Address -> Memory -> Instruction
peek (Address counter) (Memory memory) =
    case opcode of
        1 ->
            ternary Add id id position

        2 ->
            ternary Mul id id position

        3 ->
            unary Input_ position

        4 ->
            unary Output_ id

        99 ->
            Halt

        _ ->
            error ("unknow opcode: " ++ show opcode)
    where
        ( modes, opcode ) =
            let
                raw =
                    memory ! counter
            in
            ( [ (raw `div` 100) `mod` 10
              , (raw `div` 1000) `mod` 10
              , (raw `div` 10000) `mod` 10
              ]
            , raw `mod` 100
            )

        position (Position address) =
            address
        position _ =
            error "expected position mode!"

        parameters n =
            fmap (uncurry parseParameter) $
                zip modes $
                    Vector.toList $
                        Vector.slice (counter + 1) n memory

        unary instruction parse =
            case parameters 1 of
                [ a ] ->
                    instruction (parse a)

                _ ->
                    error ("invalid counter: " ++ show counter)

        ternary instruction parseA parseB parseC =
            case parameters 3 of
                [ a, b, c ] ->
                    instruction (parseA a) (parseB b) (parseC c)

                _ ->
                    error ("invalid counter: " ++ show counter)


evaluate :: Instruction -> Computer -> Maybe Computer
evaluate (Add a b c) =
    Just . advance 4 . write (operate (+) a b c)
evaluate (Mul a b c) =
    Just . advance 4 . write (operate (*) a b c)
evaluate (Input_ target) =
    Just
        . advance 2
            . (\computer ->
                let
                    ( input, computer' ) =
                        read_ computer
                in
                write (replace target input) computer'
            )
evaluate (Output_ parameter) =
    Just
        . advance 2
            . (\computer ->
                let
                    value =
                        resolve parameter (_memory computer)
                in
                print value computer
            )
evaluate Halt =
    const Nothing


operate :: (Int -> Int -> Int) -> Parameter -> Parameter -> Address -> Memory -> Memory
operate f a b c memory =
    replace c (f (resolve a memory) (resolve b memory)) memory
