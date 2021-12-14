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
        part1 input =
            let
                polymers =
                    uncurry (flip simulate) $ parse input

                countAt10 =
                    count (polymers !! 10)

                mostCommon =
                    maximum $ fmap snd countAt10

                leastCommon =
                    minimum $ fmap snd countAt10
            in
            mostCommon - leastCommon

        part2 =
            const 0


parse :: String -> ( Polymer, RuleSet )
parse input =
    case List.splitOn "\n\n" input of
        [ polymer, ruleSet ] ->
            ( parsePolymer polymer, parseRuleSet ruleSet )

        _ ->
            error "invalid input!"


data Polymer
    = Polymer [Element]


data Element
    = Element Char
    deriving (Eq, Ord)


parsePolymer :: String -> Polymer
parsePolymer =
    Polymer . fmap Element


simulate :: RuleSet -> Polymer -> [Polymer]
simulate ruleSet =
    iterate (grow ruleSet)


grow :: RuleSet -> Polymer -> Polymer
grow (RuleSet rules) (Polymer (a : b : rest)) =
    let
        Polymer end =
            grow (RuleSet rules) (Polymer (b : rest))

        insertion =
            Maybe.fromMaybe (error "pair not found!")
                (Map.lookup
                    ( a, b )
                    rules
                )
    in
    Polymer (a : insertion : end)
grow _ p =
    p


count :: Polymer -> [( Element, Int )]
count (Polymer elements) =
    Map.toList $ foldr insertOrIncrement Map.empty elements
    where
        insertOrIncrement element map =
            let
                currentCount =
                    Maybe.fromMaybe 0 $ Map.lookup element map
            in
            Map.insert element (currentCount + 1) map


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
