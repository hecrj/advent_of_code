module AdventOfCode.Y2021.Snailfish
    ( day
    ) where

import qualified AdventOfCode
import qualified Data.Char as Char
import qualified Data.List as List


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Snailfish" part1 part2
    where
        part1 =
            magnitude . foldl1 add . fmap parseNumber . lines

        part2 =
            maximum . fmap (magnitude . uncurry add) . pairs . fmap parseNumber . lines


data Number
    = Number Element Element
    deriving Eq


instance Show Number where
    show (Number a b) =
        concat [ "[", show a, ",", show b, "]" ]


data Element
    = Pair Number
    | Regular Int
    deriving Eq


instance Show Element where
    show (Pair n) =
        show n
    show (Regular n) =
        show n


parseNumber :: String -> Number
parseNumber input =
    let
        ( a, rest ) =
            parseElement (drop 1 input)

        ( b, _ ) =
            parseElement (drop 1 rest)
    in
    Number a b


parseElement :: String -> ( Element, String )
parseElement ('[' : rest) =
    let
        ( a, rest' ) =
            parseElement rest

        ( b, rest'' ) =
            parseElement (drop 1 rest')
    in
    ( Pair (Number a b), drop 1 rest'' )
parseElement rest =
    let
        n =
            takeWhile Char.isDigit rest
    in
    ( Regular (read n), drop (length n) rest )


add :: Number -> Number -> Number
add a b =
    reduce $ Number (Pair a) (Pair b)


reduce :: Number -> Number
reduce n =
    case explode n of
        Just new ->
            reduce new

        Nothing ->
            case split n of
                Just new ->
                    reduce new

                Nothing ->
                    n


explode :: Number -> Maybe Number
explode n =
    let
        flatNumber =
            components n

        nestedPair =
            List.findIndex ((==) 4 . fst) flatNumber
    in
    case nestedPair of
        Just i ->
            let
                start =
                    fmap snd $ take (i - 1) flatNumber

                end =
                    fmap snd $ drop (i + 4) flatNumber

                Value a =
                    snd $ flatNumber !! i

                Value b =
                    snd $ flatNumber !! (i + 2)

                leftNumber =
                    fmap (\p -> i - p - 2) $ List.findIndex isValue $ reverse start

                rightNumber =
                    fmap ((+) i) $ List.findIndex isValue end

                newNumber =
                    start ++ [ Value 0 ] ++ end
            in
            Just $
                parseNumber $
                    componentsToString $
                        fmap snd $
                            fmap (replaceAdd b rightNumber) $
                                fmap (replaceAdd a leftNumber) $
                                    zip [0..] $
                                        newNumber

        Nothing ->
            Nothing
    where
        replaceAdd n (Just target) ( i, Value v )
            | i == target =
                ( i, Value (v + n) )
            | otherwise =
                ( i, Value v )
        replaceAdd _ _ component =
            component


split :: Number -> Maybe Number
split (Number a b) =
    case split' a of
        Just newA ->
            Just $ Number newA b

        Nothing ->
            fmap (Number a) (split' b)
    where
        split' (Regular n)
            | n >= 10 =
                Just
                    (Pair
                        (Number (Regular (n `div` 2))
                            (Regular (n `div` 2 + n `mod` 2))
                        )
                    )
            | otherwise =
                Nothing
        split' (Pair n) =
            fmap Pair (split n)


magnitude :: Number -> Int
magnitude (Number a b) =
    3 * magnitude' a + 2 * magnitude' b
    where
        magnitude' (Pair n) =
            magnitude n
        magnitude' (Regular n) =
            n


data Component
    = Open
    | Value Int
    | Separator
    | Close
    deriving Show


isValue :: Component -> Bool
isValue (Value _) =
    True
isValue _ =
    False


components :: Number -> [( Int, Component )]
components =
    components' 0
    where
        components' i (Number a b) =
            concat
                [ [ ( 0, Open ) ]
                , element' i a
                , [ ( 0, Separator ) ]
                , element' i b
                , [ ( 0, Close ) ]
                ]

        element' i (Pair n) =
            components' (i + 1) n
        element' i (Regular value) =
            [ ( i, Value value ) ]


componentsToString :: [Component] -> String
componentsToString [] =
    []
componentsToString (c : rest) =
    case c of
        Open ->
            '[' : componentsToString rest

        Value n ->
            show n ++ componentsToString rest

        Separator ->
            ',' : componentsToString rest

        Close ->
            ']' : componentsToString rest


pairs :: [a] -> [( a, a )]
pairs xs =
    let
        pairs' =
            [ ( x, y ) | (x : rest) <- List.tails xs, y <- rest ]
    in
    concat $ fmap (\( a, b ) -> [ ( a, b ), ( b, a ) ]) pairs'
