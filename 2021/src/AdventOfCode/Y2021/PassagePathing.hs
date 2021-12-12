module AdventOfCode.Y2021.PassagePathing
    ( day
    ) where

import Data.Map.Strict (Map)
import qualified AdventOfCode
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Passage Pathing" part1 part2
    where
        part1 =
            length . paths smallCavesOnlyOnce . parse

        part2 =
            length . paths atMostOneSmallCaveTwice . parse


data System
    = System (Map Cave [Cave])


parse :: String -> System
parse =
    System . foldr insertConnection Map.empty . fmap parseConnection . lines
    where
        parseConnection input =
            case List.splitOn "-" input of
                [ from, to ] ->
                    ( parseCave from, parseCave to )

                _ ->
                    error "invalid connection!"

        insertConnection c map =
            insertOrAppend (Tuple.swap c) $ insertOrAppend c map

        insertOrAppend ( from, to ) map =
            let
                connections =
                    Maybe.fromMaybe [] $ Map.lookup from map
            in
            Map.insert from (to : connections) map


paths :: Strategy -> System -> [[Cave]]
paths strategy =
    navigate strategy Start []


navigate :: Strategy -> Cave -> [Cave] -> System -> [[Cave]]
navigate _ End path _ =
    [ reverse (End : path) ]
navigate canNavigate next path system@(System map) =
    let
        connections =
            Maybe.fromMaybe [] $ Map.lookup next map

        uniqueConnections =
            filter (flip canNavigate (next : path)) connections
    in
    concat $
        fmap
            (\connection -> navigate canNavigate connection (next : path) system)
            uniqueConnections


data Cave
    = Start
    | End
    | Small String
    | Big String
    deriving (Ord, Eq, Show)


parseCave :: String -> Cave
parseCave "start" =
    Start
parseCave "end" =
    End
parseCave name
    | all Char.isLower name =
        Small name
    | otherwise =
        Big name


isSmall :: Cave -> Bool
isSmall (Small _) =
    True
isSmall _ =
    False


type Strategy = Cave -> [Cave] -> Bool


smallCavesOnlyOnce :: Strategy
smallCavesOnlyOnce (Big _) _ =
    True
smallCavesOnlyOnce c path =
    not (List.elem c path)


atMostOneSmallCaveTwice :: Strategy
atMostOneSmallCaveTwice (Big _) _ =
    True
atMostOneSmallCaveTwice cave@(Small _) path =
    let
        isRepetitionPresent =
            any ((> 1) . length) (List.group $ List.sort (filter isSmall path))

        maxOccurrences =
            if isRepetitionPresent then 1 else 2
    in
    length (filter ((==) cave) path) < maxOccurrences
atMostOneSmallCaveTwice c path =
    not (List.elem c path)
