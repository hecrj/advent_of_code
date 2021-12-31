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

import Data.Vector (Vector, (//), (!), (!?))
import Prelude hiding (print, compare)
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Maybe as Maybe
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
replace (Address n) v (Memory memory) =
    let
        memory' =
            if Vector.length memory <= n then
                Vector.generate (n + 1024) (\i -> value (Address i) (Memory memory))

            else
                memory
    in
    Memory (memory' // [ ( n, v ) ])


value :: Address -> Memory -> Int
value (Address n) (Memory memory) =
    Maybe.fromMaybe 0 (memory !? n)


run :: Input -> Memory -> ( Memory, [Int] )
run input memory =
    let
        Computer memory' _ (Output output) _ _ =
            head $ reverse $ List.unfoldr run' $ Just $ initialize memory input
    in
    ( memory', reverse output )
    where
        run' Nothing =
            Nothing
        run' (Just computer@(Computer memory _ _ counter _)) =
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
        , _base :: Address
        }
    deriving Show


initialize :: Memory -> Input -> Computer
initialize memory input =
    Computer memory input (Output []) (Address 0) (Address 0)


write :: (Address -> Memory -> Memory) -> Computer -> Computer
write f (Computer memory input output counter base) =
    Computer (f base memory) input output counter base


advance :: Int -> Computer -> Computer
advance n (Computer memory input output (Address counter) base) =
    Computer memory input output (Address (counter + n)) base


read_ :: Computer -> ( Int, Computer )
read_ (Computer memory (Input input) output counter base) =
    ( head input, Computer memory (Input (tail input)) output counter base )


print :: Int -> Computer -> Computer
print value (Computer memory input (Output output) counter base) =
    Computer memory input (Output (value : output)) counter base


jump :: Address -> Computer -> Computer
jump address (Computer memory input output _ base) =
    Computer memory input output address base


adjust :: Int -> Computer -> Computer
adjust offset (Computer memory input output counter (Address base)) =
    Computer memory input output counter (Address (base + offset))


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
    = Add Parameter Parameter Position
    | Mul Parameter Parameter Position
    | Input_ Position
    | Output_ Parameter
    | JumpIfTrue Parameter Parameter
    | JumpIfFalse Parameter Parameter
    | LessThan Parameter Parameter Position
    | Equals Parameter Parameter Position
    | AdjustBase Parameter
    | Halt
    deriving Show


data Parameter
    = Position Position
    | Immediate Int
    deriving Show


data Position
    = Absolute Address
    | Relative Address
    deriving Show


toAbsolute :: Address -> Position -> Address
toAbsolute _ (Absolute address) =
    address
toAbsolute (Address base) (Relative (Address address)) =
    Address (base + address)


parseParameter :: Int -> Int -> Parameter
parseParameter 0 =
    Position . Absolute . Address
parseParameter 1 =
    Immediate
parseParameter 2 =
    Position . Relative . Address
parseParameter n =
    error ("invalid parameter mode: " ++ show n)


resolve :: Parameter -> Address -> Memory -> Int
resolve (Position position) base =
    value (toAbsolute base position)
resolve (Immediate value) _ =
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

        5 ->
            binary JumpIfTrue id id

        6 ->
            binary JumpIfFalse id id

        7 ->
            ternary LessThan id id position

        8 ->
            ternary Equals id id position

        9 ->
            unary AdjustBase id

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

        binary instruction parseA parseB =
            case parameters 2 of
                [ a, b ] ->
                    instruction (parseA a) (parseB b)

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
                write (const $ replace (toAbsolute (_base computer') target) input) computer'
            )
evaluate (Output_ parameter) =
    Just
        . advance 2
            . (\computer ->
                let
                    value =
                        resolve parameter (_base computer) (_memory computer)
                in
                print value computer
            )
evaluate (JumpIfTrue a b) =
    Just
        . (\computer ->
            let
                value =
                    resolve a (_base computer) (_memory computer)

                address =
                    resolve b (_base computer) (_memory computer)
            in
            if value /= 0 then
                jump (Address address) computer

            else
                advance 3 computer
        )
evaluate (JumpIfFalse a b) =
    Just
        . (\computer ->
            let
                value =
                    resolve a (_base computer) (_memory computer)

                address =
                    resolve b (_base computer) (_memory computer)
            in
            if value == 0 then
                jump (Address address) computer

            else
                advance 3 computer
        )
evaluate (LessThan a b target) =
    Just . advance 4 . write (operate (compare (<)) a b target)
evaluate (Equals a b target) =
    Just . advance 4 . write (operate (compare (==)) a b target)
evaluate (AdjustBase offset) =
    Just
        . advance 2
            . (\computer ->
                let
                    value =
                        resolve offset (_base computer) (_memory computer)
                in
                adjust value computer
            )
evaluate Halt =
    const Nothing


operate :: (Int -> Int -> Int) -> Parameter -> Parameter -> Position -> Address -> Memory -> Memory
operate f a b c base memory =
    replace (toAbsolute base c) (f (resolve a base memory) (resolve b base memory)) memory


compare :: (Int -> Int -> Bool) -> Int -> Int -> Int
compare f a b =
    if f a b then 1 else 0
