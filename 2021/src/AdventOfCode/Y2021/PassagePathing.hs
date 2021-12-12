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
    AdventOfCode.day "Passage Pathing" (length . paths . parse) (const 0)


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


paths :: System -> [[Cave]]
paths =
    navigate Start []


navigate :: Cave -> [Cave] -> System -> [[Cave]]
navigate End path _ =
    [ reverse (End : path) ]
navigate next path system@(System map) =
    let
        connections =
            Maybe.fromMaybe [] $ Map.lookup next map

        uniqueConnections =
            filter (\c -> canRepeat c || not (List.elem c path)) connections
    in
    concat $
        fmap
            (\connection -> navigate connection (next : path) system)
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


canRepeat :: Cave -> Bool
canRepeat (Big _) =
    True
canRepeat _ =
    False
