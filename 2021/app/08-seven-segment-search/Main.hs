module Main where

import qualified Data.List.Split as List


main :: IO ()
main = do
    entries <- fmap (fmap parseEntry . lines) getContents
    putStrLn $ show $ sum (fmap uniqueOutputDigits entries)


data Entry
    = Entry { patterns :: [Pattern], output :: [Pattern] }
    deriving Show


parseEntry :: String -> Entry
parseEntry input =
    case List.splitOn " | " input of
        [ patterns, output ] ->
            Entry
                (fmap parsePattern (List.splitOn " " patterns))
                (fmap parsePattern (List.splitOn " " output))

        _ ->
            error "invalid entry!"


uniqueOutputDigits :: Entry -> Int
uniqueOutputDigits (Entry _ output) =
    length (filter isUnique output)


data Signal
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    deriving (Show, Eq, Ord)


parseSignal :: Char -> Signal
parseSignal 'a' =
    A
parseSignal 'b' =
    B
parseSignal 'c' =
    C
parseSignal 'd' =
    D
parseSignal 'e' =
    E
parseSignal 'f' =
    F
parseSignal 'g' =
    G
parseSignal _ =
    error "invalid segment!"


data Pattern
    = Pattern [Signal]
    deriving Show


parsePattern :: String -> Pattern
parsePattern =
    Pattern . fmap parseSignal


isUnique :: Pattern -> Bool
isUnique (Pattern signals) =
    length signals `elem` [ 2, 3, 4, 7 ]


data Digit
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine


digitPattern :: Digit -> Pattern
digitPattern Zero =
    Pattern [ A, B, C, E, F, G ]
digitPattern One =
    Pattern [ C, F ]
digitPattern Two =
    Pattern [ A, C, D, E, G ]
digitPattern Three =
    Pattern [ A, C, D, F, G ]
digitPattern Four =
    Pattern [ B, C, D, F ]
digitPattern Five =
    Pattern [ A, B, D, F, G ]
digitPattern Six =
    Pattern [ A, B, D, E, F, G ]
digitPattern Seven =
    Pattern [ A, C, F ]
digitPattern Eight =
    Pattern [ A, B, C, D, E, F, G ]
digitPattern Nine =
    Pattern [ A, B, C, D, F, G ]
