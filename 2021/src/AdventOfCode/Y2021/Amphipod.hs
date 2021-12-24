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
            fst . Maybe.fromMaybe (error "no way to organize found!") . organize . parse

        part2 =
            const 0


data Building
    = Building
        { _amphipods :: [Amphipod]
        }
    deriving (Eq, Ord, Generic)


instance Show Building where
    show (Building amphipods) =
        concat $
            List.intersperse "\n"
                [ []
                , replicate 13 '#'
                , concat [ "#", hallway, "#" ]
                , concat
                    [ "###"
                    , pure $ room Amber Top
                    , "#"
                    , pure $ room Bronze Top
                    , "#"
                    , pure $ room Copper Top
                    , "#"
                    , pure $ room Desert Top
                    , "###"
                    ]
                , concat
                    [ "  #"
                    , pure $ room Amber Bottom
                    , "#"
                    , pure $ room Bronze Bottom
                    , "#"
                    , pure $ room Copper Bottom
                    , "#"
                    , pure $ room Desert Bottom
                    , "#"
                    ]
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


parse :: String -> Building
parse input =
    Building $
        List.sort $
            concat
                [ parseRoom 0 Amber
                , parseRoom 1 Bronze
                , parseRoom 2 Copper
                , parseRoom 3 Desert
                ]
    where
        room n =
            concat $ drop (2 * n) $ drop 3 $ List.transpose $ drop 2 $ lines input

        parseRoom n roomKind =
            case room n of
                (top : bottom : _) ->
                    [ flip Amphipod (Idle roomKind Top) (parseKind top)
                    , let
                        kind =
                            parseKind bottom
                    in
                    if kind == roomKind then
                        Amphipod kind (Arrived Bottom)

                    else
                        flip Amphipod (Idle roomKind Bottom) (parseKind bottom)
                    ]

                _ ->
                    error "invalid room!"


complete :: Building
complete =
    Building $
        List.sort $
            concat $
                fmap
                    (\kind ->
                        [ Amphipod kind (Arrived Top)
                        , Amphipod kind (Arrived Bottom)
                        ]
                    )
                    [ Amber, Bronze, Copper, Desert ]


organize :: Building -> Maybe ( Int, [Building] )
organize building =
    Path.search building complete movement (const 0)
    where
        movement (Building amphipods) =
            concat $
                fmap
                    (\pod -> do
                        ( Building pods, cost ) <- moves (Building (List.delete pod amphipods)) pod
                        [ ( Building (List.sort pods), cost ) ]
                    )
                    amphipods


moves :: Building -> Amphipod -> [( Building, Int )]
moves _ (Amphipod _ (Arrived _)) =
    []
moves (Building others) (Amphipod kind (Idle room position))
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
                [ ( Building (Amphipod kind (Arrived position) : others), 0 ) ]

            else
                []

        moveToHallway cell
            | isPathToHallwayBlocked cell =
                Nothing
            | otherwise =
                Just
                    ( Building (Amphipod kind (Moving cell) : others)
                    , costToHallway room kind cell position
                    )

        isRoomExitBlocked =
            case position of
                Top ->
                    False

                Bottom ->
                    any (idling room) others || any (reached room) others

        isPathToHallwayBlocked cell =
            any (blocking (cells room cell)) others
moves (Building others) (Amphipod kind (Moving cell))
    | isHallwayBlocked || isRoomBlocked =
        []
    | otherwise =
        [ ( Building (Amphipod kind (Arrived destinationPosition) : others)
          , costToRoom kind cell destinationPosition
          )
        ]
    where
        isHallwayBlocked =
            any (blocking (cells kind cell)) others

        isRoomBlocked =
            any (idling kind) others

        destinationPosition =
            if any (reached kind) others then
                Top

            else
                Bottom


data Amphipod
    = Amphipod
        { kind :: Kind
        , state :: State
        }
    deriving (Eq, Ord, Generic, Show)


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
    = Idle Kind Position
    | Moving Int
    | Arrived Position
    deriving (Eq, Ord, Generic, Show)


data Position
    = Top
    | Bottom
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


costToRoom :: Kind -> Int -> Position -> Int
costToRoom kind cell position =
    costToHallway kind kind cell position


costToHallway :: Kind -> Kind -> Int -> Position -> Int
costToHallway room kind cell position =
    let
        roomSteps =
            case position of
                Top ->
                    1

                Bottom ->
                    2

        hallwaySteps =
            abs (cell - hallwayLocation room)

        steps =
            roomSteps + hallwaySteps
    in
    steps * cost kind


instance Hashable Building where


instance Hashable Amphipod where


instance Hashable Kind where


instance Hashable State where


instance Hashable Position where
