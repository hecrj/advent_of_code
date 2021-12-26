module AdventOfCode.Y2021.ArithmeticLogicUnit
    ( day
    ) where

import qualified AdventOfCode
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Maybe as Maybe


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Arithmetic Logic Unit" part1 part2
    where
        part1 =
            head . validate highModels . parse

        part2 =
            head . validate lowModels . parse

        validate models monad =
            fmap toInt $ List.filter ((==) 0 . z . run monad) models

        toInt =
            sum . fmap (uncurry (*)) . zip (fmap ((^) 10) [0..]) . reverse

        highModels =
            [
                [ 9
                , 4
                , 3
                , 9
                , 9
                , 8
                , 9
                , 8
                , 9
                , n10
                , n11
                , n12
                , n13
                , n14
                ]
                | n10 <- enumFromThenTo 9 8 1
                , n11 <- enumFromThenTo 9 8 1
                , n12 <- enumFromThenTo 9 8 1
                , n13 <- enumFromThenTo 9 8 1
                , n14 <- enumFromThenTo 9 8 1
            ]

        lowModels =
            [
                [ 2
                , 1
                , 1
                , 7
                , 6
                , 1
                , 2
                , 1
                , 6
                , n10
                , n11
                , n12
                , n13
                , n14
                ]
                | n10 <- [1..9]
                , n11 <- [1..9]
                , n12 <- [1..9]
                , n13 <- [1..9]
                , n14 <- [1..9]
            ]


data Program
    = Program [Instruction]
    deriving Show


parse :: String -> Program
parse =
    Program . fmap parseInstruction . lines


run :: Program -> [Int] -> State
run (Program instructions) input =
    foldl (flip evaluate) (initialState input) instructions


data Instruction
    = Input Variable
    | Add Variable Operand
    | Mul Variable Operand
    | Div Variable Operand
    | Mod Variable Operand
    | Equal Variable Operand
    deriving Show


parseInstruction :: String -> Instruction
parseInstruction input =
    case List.splitOn " " input of
        [ "inp", x ] ->
            Input (parseVariable x)

        [ "add", a, b ] ->
            Add (parseVariable a) (parseOperand b)

        [ "mul", a, b ] ->
            Mul (parseVariable a) (parseOperand b)

        [ "div", a, b ] ->
            Div (parseVariable a) (parseOperand b)

        [ "mod", a, b ] ->
            Mod (parseVariable a) (parseOperand b)

        [ "eql", a, b ] ->
            Equal (parseVariable a) (parseOperand b)

        _ ->
            error "invalid instruction!"


evaluate :: Instruction -> State -> State
evaluate (Input variable) state =
    let
        ( value, state' ) =
            readInput state
    in
    store variable value state'
evaluate (Add variable operand) state =
    operate (+) variable operand state
evaluate (Mul variable operand) state =
    operate (*) variable operand state
evaluate (Div variable operand) state =
    operate (div) variable operand state
evaluate (Mod variable operand) state =
    operate (mod) variable operand state
evaluate (Equal variable operand) state =
    operate (\a b -> if a == b then 1 else 0) variable operand state


data Variable
    = W
    | X
    | Y
    | Z
    deriving Show


parseVariable :: String -> Variable
parseVariable =
    Maybe.fromMaybe (error "invalid variable!") . maybeParseVariable


maybeParseVariable :: String -> Maybe Variable
maybeParseVariable "w" =
    Just W
maybeParseVariable "x" =
    Just X
maybeParseVariable "y" =
    Just Y
maybeParseVariable "z" =
    Just Z
maybeParseVariable _ =
    Nothing


data Operand
    = Variable Variable
    | Number Int
    deriving Show


parseOperand :: String -> Operand
parseOperand operand =
    case maybeParseVariable operand of
        Just variable ->
            Variable variable

        Nothing ->
            Number (read operand)


data State
    = State
        { w :: Int
        , x :: Int
        , y :: Int
        , z :: Int
        , input :: [Int]
        }
    deriving Show


initialState :: [Int] -> State
initialState =
    State 0 0 0 0


readInput :: State -> ( Int, State )
readInput (State w x y z input) =
    ( head input, State w x y z (tail input) )


store :: Variable -> Int -> State -> State
store W number (State _ x y z input) =
    State number x y z input
store X number (State w _ y z input) =
    State w number y z input
store Y number (State w x _ z input) =
    State w x number z input
store Z number (State w x y _ input) =
    State w x y number input


operate :: (Int -> Int -> Int) -> Variable -> Operand -> State -> State
operate operation variable operand state =
    let
        result =
            operation (get variable state) (get' operand state)
    in
    store variable result state


get :: Variable -> State -> Int
get W (State w _ _ _ _) =
    w
get X (State _ x _ _ _) =
    x
get Y (State _ _ y _ _) =
    y
get Z (State _ _ _ z _) =
    z


get' :: Operand -> State -> Int
get' operand state =
    case operand of
        Variable variable ->
            get variable state

        Number number ->
            number
