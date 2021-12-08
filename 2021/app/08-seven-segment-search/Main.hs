module Main where

import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Map.Strict as Map


main :: IO ()
main = do
    entries <- fmap (fmap parseEntry . lines) getContents
    putStrLn $ show $ sum (fmap uniqueOutputDigits entries)
    putStrLn $ show $ sum (fmap translateOutput entries)


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


translateOutput :: Entry -> Int
translateOutput (Entry patterns output) =
    let
        translation =
            fromMaybe (error "translation not found!") $
                List.find translatesAll allTranslations

        digits =
            catMaybes $ fmap (translate translation) output
    in
    sum (fmap (\( i, d ) -> 10 ^ i * toInt d) (zip [0..] (reverse digits)))
    where
        translatesAll translation =
            all isJust (fmap (translate translation) patterns)


uniqueOutputDigits :: Entry -> Int
uniqueOutputDigits (Entry _ output) =
    length (filter isUnique output)


data Translation
    = Translation (Map Signal Signal)
    deriving Show


allTranslations :: [Translation]
allTranslations =
    fmap translationFromList (List.permutations (enumFrom A))


translationFromList :: [Signal] -> Translation
translationFromList =
    Translation . Map.fromList . zip (enumFrom A)


translate :: Translation -> Pattern -> Maybe Digit
translate (Translation map) (Pattern signals) =
    digitFromPattern (Pattern (fmap replace signals))
    where
        replace signal =
            fromMaybe (error "invalid translation!") (Map.lookup signal map)


data Signal
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    deriving (Show, Eq, Ord, Enum)


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
    deriving (Show, Eq)


parsePattern :: String -> Pattern
parsePattern =
    Pattern . fmap parseSignal


isUnique :: Pattern -> Bool
isUnique (Pattern signals) =
    length signals `elem` [ 2, 3, 4, 7 ]


patternLength :: Pattern -> Int
patternLength =
    length . patternSignals


patternSignals :: Pattern -> [Signal]
patternSignals (Pattern signals) =
    signals


sort :: Pattern -> Pattern
sort =
    Pattern . List.sort . patternSignals


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
    deriving Enum


digitFromPattern :: Pattern -> Maybe Digit
digitFromPattern p =
    List.find (\d -> digitPattern d == sort p) (enumFrom Zero)


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


toInt :: Digit -> Int
toInt Zero =
    0
toInt One =
    1
toInt Two =
    2
toInt Three =
    3
toInt Four =
    4
toInt Five =
    5
toInt Six =
    6
toInt Seven =
    7
toInt Eight =
    8
toInt Nine =
    9


digitsWithLength :: Int -> [Digit]
digitsWithLength n =
    filter (\d -> patternLength (digitPattern d) == n) (enumFrom Zero)
