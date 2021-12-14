module AdventOfCode.Y2021.ExtendedPolymerization
    ( day
    ) where

import Data.Map (Map)
import qualified AdventOfCode
import qualified Data.List.Split as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Extended Polymerization" part1 part2
    where
        part1 =
            run 10

        part2 =
            run 40

        run steps input =
            let
                polymers =
                    uncurry (flip simulate) $ parse input

                countAtSteps =
                    count (polymers !! steps)

                mostCommon =
                    maximum $ fmap snd countAtSteps

                leastCommon =
                    minimum $ fmap snd countAtSteps
            in
            mostCommon - leastCommon


parse :: String -> ( Polymer, RuleSet )
parse input =
    case List.splitOn "\n\n" input of
        [ polymer, ruleSet ] ->
            ( parsePolymer polymer, parseRuleSet ruleSet )

        _ ->
            error "invalid input!"


data Polymer
    = Polymer
        { _elements :: Map Element Int
        , _pairs :: PairMap
        }


type PairMap = Map ( Element, Element ) Int


lookup' :: Ord a => a -> Map a Int -> Int
lookup' key =
    Maybe.fromMaybe 0 . Map.lookup key


increment :: Ord a => Int -> a -> Map a Int -> Map a Int
increment n key map =
    Map.insert key (lookup' key map + n) map


data Element
    = Element Char
    deriving (Eq, Ord)


parsePolymer :: String -> Polymer
parsePolymer input =
    let
        ( elements, pairs ) =
            parseMap Map.empty Map.empty input
    in
    Polymer elements pairs
    where
        parseMap elements pairs (a : b : rest) =
            let
                ( newElements, newPairs ) =
                    parseMap elements pairs (b : rest)
            in
            ( increment 1 (Element a) newElements
            , increment 1 ( Element a, Element b ) newPairs
            )
        parseMap elements pairs [ a ] =
            ( increment 1 (Element a) elements, pairs )
        parseMap elements pairs [] =
            ( elements, pairs )


simulate :: RuleSet -> Polymer -> [Polymer]
simulate ruleSet =
    iterate (grow ruleSet)


grow :: RuleSet -> Polymer -> Polymer
grow rules (Polymer elements pairs) =
    uncurry Polymer $
        foldr (grow' rules) ( elements, Map.empty ) (Map.toList pairs)


grow' ::
    RuleSet
    -> ( ( Element, Element ), Int )
    -> ( Map Element Int, PairMap )
    -> ( Map Element Int, PairMap )
grow' (RuleSet rules) ( ( a, b ), count ) ( elements, pairs ) =
    let
        insertion =
            Maybe.fromMaybe (error "pair not found!")
                (Map.lookup
                    ( a, b )
                    rules
                )
    in
    ( increment count insertion elements
    , increment count ( a, insertion ) $ increment count ( insertion, b ) pairs
    )


count :: Polymer -> [( Element, Int )]
count (Polymer elements _) =
    Map.toList elements


data RuleSet
    = RuleSet (Map ( Element, Element ) Element)


parseRuleSet :: String -> RuleSet
parseRuleSet =
    RuleSet . Map.fromList . fmap parseRule . lines
    where
        parseRule input =
            case List.splitOn " -> " input of
                [ [ a, b ], [ target ] ] ->
                    ( ( Element a, Element b ), Element target )

                _ ->
                    error "invalid rule!"
