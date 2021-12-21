{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
module AdventOfCode.Y2021.DiracDice
    ( day
    ) where

import Data.Map (Map)
import qualified AdventOfCode
import qualified AdventOfCode.Tuple as Tuple
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Dirac Dice" part1 part2
    where
        part1 =
            losingScore . head . reverse . play deterministic100Die . parse

        part2 =
            maxWins . head . reverse . playMultiverse . toMultiverse . parse


parse :: String -> Game
parse =
    Game 0 . fmap parsePlayer . lines


data Player
    = Player
        { _position :: Int
        , _score :: Int
        }
    deriving (Eq, Ord)


parsePlayer :: String -> Player
parsePlayer input =
    case List.splitOn ": " input of
        [ _, position ] ->
            Player (read position) 0

        _ ->
            error "invalid player!"


move :: Int -> Player -> Player
move result (Player position score) =
    let
        newPosition =
            (position + result - 1) `mod` 10 + 1

        newScore =
            score + newPosition
    in
    Player newPosition newScore


newtype Die
    = Die (Int -> Int)


deterministic100Die :: Die
deterministic100Die =
    Die ((+) 1 . flip (mod) 100)


data Game
    = Game
        { _turn :: Int
        , _players :: [Player]
        }


play :: Die -> Game -> [Game]
play die =
    List.unfoldr (fmap Tuple.dupe . next die)


next :: Die -> Game -> Maybe Game
next _ (Game _ []) =
    Nothing
next (Die die) (Game turn (player : rest))
    | all (flip (<) 1000) (fmap _score (player : rest)) =
        let
            roll =
                turn * 3

            result =
                sum $ fmap die [roll..roll + 2]
        in
        Just $ Game (turn + 1) (rest ++ [ move result player ])
    | otherwise =
        Nothing


losingScore :: Game -> Int
losingScore (Game _ []) =
    0
losingScore (Game turn (loser : _)) =
    turn * 3 * _score loser


data Multiverse
    = Multiverse
        { _turn :: PlayerId
        , _wins :: ( Int, Int )
        , _universes :: Map Universe Int
        }


data Universe
    = Universe Player Player
    deriving (Eq, Ord)


data PlayerId
    = One
    | Two


toggle :: PlayerId -> PlayerId
toggle One =
    Two
toggle Two =
    One


toMultiverse :: Game -> Multiverse
toMultiverse (Game _ [ a, b ]) =
    Multiverse One ( 0, 0 ) (Map.singleton (Universe a b) 1)
toMultiverse (Game _ _) =
    error "multiverse can only have two players!"


playMultiverse :: Multiverse -> [Multiverse]
playMultiverse =
    List.unfoldr (fmap Tuple.dupe . nextMultiverse)


nextMultiverse :: Multiverse -> Maybe Multiverse
nextMultiverse (Multiverse turn ( wins1, wins2 ) universes)
    | Map.null universes =
        Nothing
    | otherwise =
        let
            newUniverses =
                concat $ fmap (moveAll turn universes) quantumRolls

            pendingUniverses =
                filter (not . isFinished . fst) newUniverses

            gamesEnded =
                sum $ fmap snd $ filter (isFinished . fst) newUniverses

            newWins =
                case turn of
                    One ->
                        ( wins1 + gamesEnded, wins2 )

                    Two ->
                        ( wins1, wins2 + gamesEnded )
        in
        Just $ Multiverse (toggle turn) newWins (foldr merge Map.empty pendingUniverses)
    where
        moveAll turn universes result =
            fmap ((\( universe, amount ) -> ( operate (move result) turn universe, amount )))
                (Map.toList universes)

        merge ( universe, amount ) map =
            Map.insert universe (Maybe.fromMaybe 0 (Map.lookup universe map) + amount) map


maxWins :: Multiverse -> Int
maxWins (Multiverse _ ( a, b ) _) =
    max a b


operate :: (Player -> Player) -> PlayerId -> Universe -> Universe
operate f One (Universe a b) =
    Universe (f a) b
operate f Two (Universe a b) =
    Universe a (f b)


isFinished :: Universe -> Bool
isFinished (Universe a b) =
    _score a >= 21 || _score b >= 21


quantumRolls :: [Int]
quantumRolls =
    fmap sum $
        [
            [ a, b, c ]
            | a <- [ 1, 2, 3 ]
            , b <- [ 1, 2, 3 ]
            , c <- [ 1, 2, 3 ]
        ]
