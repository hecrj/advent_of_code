{-# LANGUAGE DeriveGeneric #-}
module AdventOfCode.Y2021.Amphipod
    ( day
    ) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Prelude hiding (Left, Right)
import qualified AdventOfCode
import qualified AdventOfCode.Path as Path
import qualified Data.List as List
import qualified Data.Maybe as Maybe


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Amphipod" part1 part2
    where
        part1 =
            solve

        part2 input =
            let
                ( start, end ) =
                    List.splitAt 3 $ lines input

                unfolded =
                    concat $
                        List.intersperse "\n" $
                            concat
                                [ start
                                , [ "  #D#C#B#A#" ]
                                , [ "  #D#B#A#C#" ]
                                , end
                                ]
            in
            solve unfolded

        solve =
            fst . Maybe.fromMaybe (error "no way to organize found!") . organize . parse


data Building
    = Building
        { _size :: Int
        , _amphipods :: [Amphipod]
        }
    deriving (Eq, Ord, Generic)


parse :: String -> Building
parse input =
    Building size $ List.sort $ concat rooms
    where
        rooms =
            [ parseRoom 0 Amber
            , parseRoom 1 Bronze
            , parseRoom 2 Copper
            , parseRoom 3 Desert
            ]

        size =
            length $ List.transpose rooms

        room n =
            concat $ take 1 $ drop (2 * n) $ drop 3 $ List.transpose $ drop 2 $ lines input

        parseRoom n roomKind =
            fmap (uncurry (parseAmphipod roomKind)) $
                zip [0..] $
                    reverse $
                        drop 1 $
                            reverse (room n)


complete :: Int -> Building
complete size =
    Building size $
        List.sort $
            concat $
                fmap
                    (\kind ->
                        fmap (Amphipod kind . Arrived) [0..size - 1]
                    )
                    [ Amber, Bronze, Copper, Desert ]


organize :: Building -> Maybe ( Int, [Building] )
organize building@(Building size _) =
    Path.search building (complete size) movement (const 0)
    where
        movement (Building size amphipods) =
            concat $
                fmap
                    (\pod -> do
                        ( Building _ pods, cost ) <- moves (Building size (List.delete pod amphipods)) pod
                        [ ( Building size (List.sort pods), cost ) ]
                    )
                    amphipods


moves :: Building -> Amphipod -> [( Building, Int )]
moves _ (Amphipod _ (Arrived _)) =
    []
moves (Building size others) (Amphipod kind (Idle room position))
    | isRoomExitBlocked =
        arrivedMoves
    | otherwise =
        concat $
            [ arrivedMoves
            , Maybe.catMaybes $ fmap moveToHallway hallwayCells
            ]
    where
        arrivedMoves =
            if kind == room then
                [ ( Building size (Amphipod kind (Arrived position) : others), 0 ) ]

            else
                []

        moveToHallway cell
            | isPathToHallwayBlocked cell =
                Nothing
            | otherwise =
                Just
                    ( Building size (Amphipod kind (Moving cell) : others)
                    , costToHallway room kind cell position
                    )

        isRoomExitBlocked =
            Maybe.isJust $
                List.find (flip (<) position) $
                    Maybe.catMaybes $
                        fmap maybePosition $
                            List.filter (isInside room) others

        isPathToHallwayBlocked cell =
            any (blocking (cells room cell)) others
moves (Building size others) (Amphipod kind (Moving cell))
    | isHallwayBlocked || isRoomBlocked =
        []
    | otherwise =
        [ ( Building size (Amphipod kind (Arrived destinationPosition) : others)
          , costToRoom kind cell destinationPosition
          )
        ]
    where
        isHallwayBlocked =
            any (blocking (cells kind cell)) others

        isRoomBlocked =
            any (idling kind) others

        destinationPosition =
            size - length (List.filter (reached kind) others) - 1


data Amphipod
    = Amphipod
        { kind :: Kind
        , state :: State
        }
    deriving (Eq, Ord, Generic, Show)


parseAmphipod :: Kind -> Int -> Char -> Amphipod
parseAmphipod room position kind =
    Amphipod (parseKind kind) (Idle room position)


idling :: Kind -> Amphipod -> Bool
idling kind (Amphipod _ (Idle room _)) =
    kind == room
idling _ _ =
    False


moving :: Int -> Amphipod -> Bool
moving cell (Amphipod _ (Moving n)) =
    cell == n
moving _ _ =
    False


cells :: Kind -> Int -> [Int]
cells room target =
    let
        location =
            hallwayLocation room

        distance =
            target - location
    in
    enumFromThenTo location (location + signum distance) target


blocking :: [Int] -> Amphipod -> Bool
blocking cells (Amphipod _ (Moving n)) =
    List.elem n cells
blocking _ _ =
    False


reached :: Kind -> Amphipod -> Bool
reached kind (Amphipod room (Arrived _)) =
    kind == room
reached _ _ =
    False


isInside :: Kind -> Amphipod -> Bool
isInside room pod =
    reached room pod || idling room pod


maybePosition :: Amphipod -> Maybe Int
maybePosition (Amphipod _ (Idle _ position)) =
    Just position
maybePosition (Amphipod _ (Arrived position)) =
    Just position
maybePosition _ =
    Nothing


data Kind
    = Amber
    | Bronze
    | Copper
    | Desert
    deriving (Eq, Ord, Generic, Show)


parseKind :: Char -> Kind
parseKind 'A' =
    Amber
parseKind 'B' =
    Bronze
parseKind 'C' =
    Copper
parseKind 'D' =
    Desert
parseKind c =
    error $ "invalid kind: " ++ [ c ]


toChar :: Kind -> Char
toChar Amber =
    'A'
toChar Bronze =
    'B'
toChar Copper =
    'C'
toChar Desert =
    'D'


data State
    = Idle Kind Int
    | Moving Int
    | Arrived Int
    deriving (Eq, Ord, Generic, Show)


cost :: Kind -> Int
cost Amber =
    1
cost Bronze =
    10
cost Copper =
    100
cost Desert =
    1000


hallwayCells :: [Int]
hallwayCells =
    [ 0, 1, 3, 5, 7, 9, 10 ]


hallwayLocation :: Kind -> Int
hallwayLocation Amber =
    2
hallwayLocation Bronze =
    4
hallwayLocation Copper =
    6
hallwayLocation Desert =
    8


costToRoom :: Kind -> Int -> Int -> Int
costToRoom kind cell position =
    costToHallway kind kind cell position


costToHallway :: Kind -> Kind -> Int -> Int -> Int
costToHallway room kind cell position =
    let
        roomSteps =
            position + 1

        hallwaySteps =
            abs (cell - hallwayLocation room)

        steps =
            roomSteps + hallwaySteps
    in
    steps * cost kind


instance Show Building where
    show (Building size amphipods) =
        concat $
            List.intersperse "\n"
                [ []
                , replicate 13 '#'
                , concat [ "#", hallway, "#" ]
                , concat $ fmap rooms [0..size - 1]
                , concat [ "  ", replicate 9 '#' ]
                , []
                ]
        where
            hallway =
                fmap halwayCell [0..10]

            halwayCell cell =
                case List.find (moving cell) amphipods of
                    Just (Amphipod kind _) ->
                        toChar kind

                    Nothing ->
                        '.'

            rooms position =
                concat
                    [ "###"
                    , pure $ room Amber position
                    , "#"
                    , pure $ room Bronze position
                    , "#"
                    , pure $ room Copper position
                    , "#"
                    , pure $ room Desert position
                    , "###"
                    ]

            room kind position =
                case List.find (insideRoom kind position) amphipods of
                    Just (Amphipod kind _) ->
                        toChar kind

                    Nothing ->
                        '.'

            insideRoom room position pod =
                case pod of
                    Amphipod _ (Idle r p) ->
                        r == room && p == position

                    Amphipod r (Arrived p) ->
                        r == room && p == position

                    _ ->
                        False


instance Hashable Building where


instance Hashable Amphipod where


instance Hashable Kind where


instance Hashable State where
