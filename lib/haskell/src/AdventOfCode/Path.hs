module AdventOfCode.Path
    ( Heuristic
    , Movement
    , search
    ) where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Maybe as Maybe
import qualified Data.PQueue.Prio.Min as MinPQueue


type Movement a = a -> [( a, Int )]


type Heuristic a = a -> Int


search :: (Eq a, Hashable a) => a -> a -> Movement a -> Heuristic a -> Maybe ( Int, [a] )
search start end movement heuristic =
    aStar
        (MinPQueue.singleton (heuristic start) ( start, 0 ))
        HashSet.empty
        (HashMap.singleton start 0)
        HashMap.empty
    where
        aStar pending visited distances tracks
            | MinPQueue.null pending =
                Nothing
            | current == end =
                Just ( distance, buildPath tracks end )
            | HashSet.member current visited =
                aStar pending' visited distances tracks
            | otherwise =
                aStar pending'' visited' distances' tracks'
            where
                ( current, distance ) =
                    snd (MinPQueue.findMin pending)

                pending' =
                    MinPQueue.deleteMin pending

                visited' =
                    HashSet.insert current visited

                neighbors =
                    let
                        isCloser p distance =
                            distance < (Maybe.fromMaybe maxBound $ HashMap.lookup p distances)

                        withDistance p cost =
                            ( p, distance + cost )
                    in
                    filter
                        (\( p, _ ) ->
                            not (HashSet.member p visited')
                        ) $
                        filter (uncurry isCloser) $
                            fmap (uncurry withDistance) $
                                movement current

                pending'' =
                    let
                        insert pending ( p, d ) =
                            MinPQueue.insert (d + heuristic p) ( p, d ) pending
                    in
                    foldl' insert pending' neighbors

                distances' =
                    foldl' (\m ( s, g ) -> HashMap.insert s g m) distances neighbors

                tracks' =
                    foldl' (\m ( s, _ ) -> HashMap.insert s current m) tracks neighbors

                buildPath tracks node =
                    if HashMap.member node tracks then
                        buildPath tracks (Maybe.fromJust . HashMap.lookup node $ tracks) ++ [ node ]

                    else
                        [ node ]
