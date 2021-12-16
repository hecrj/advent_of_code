module AdventOfCode.Y2021.PacketDecoder
    ( day
    ) where

import qualified AdventOfCode
import qualified Data.List as List
import qualified Data.Maybe as Maybe


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Packet Decoder" part1 part2
    where
        part1 =
            sum . versions . parse

        part2 =
            evaluate . parse


parse :: String -> Packet
parse =
    fst . Maybe.fromJust . decodePacket . concat . parseBits


data Packet
    = Literal { _version :: Int, _value :: Int }
    | Operator { _version :: Int, operation :: Operation, packets :: [Packet] }
    deriving Show


data Operation
    = Sum
    | Product
    | Minimum
    | Maximum
    | GreaterThan
    | LessThan
    | EqualTo
    deriving Show


decodePacket :: [Bit] -> Maybe ( Packet, [Bit] )
decodePacket [] =
    Nothing
decodePacket (v1 : v2 : v3 : id1 : id2 : id3 : rest) =
    let
        version =
            toDecimal [ v1, v2, v3 ]

        typeId =
            toDecimal [ id1, id2, id3 ]
    in
    case typeId of
        4 ->
            let
                ( value, rest' ) =
                    decodeValue rest
            in
            Just ( Literal version value, rest' )

        _ ->
            let
                operation =
                    case typeId of
                        0 ->
                            Sum

                        1 ->
                            Product

                        2 ->
                            Minimum

                        3 ->
                            Maximum

                        5 ->
                            GreaterThan

                        6 ->
                            LessThan

                        7 ->
                            EqualTo

                        _ ->
                            error "invalid operator!"
            in
            case rest of
                Zero : rest ->
                    let
                        length =
                            toDecimal (take 15 rest)

                        rest' =
                            drop 15 rest

                        packetBits =
                            take length rest'

                        packets =
                            List.unfoldr decodePacket packetBits
                    in
                    Just ( Operator version operation packets, drop length rest' )

                One : rest ->
                    let
                        packetCount =
                            toDecimal (take 11 rest)

                        rest' =
                            drop 11 rest

                        packets =
                            take packetCount $
                                List.unfoldr
                                    (\bits ->
                                        let
                                            result =
                                                decodePacket bits
                                        in
                                        fmap
                                            (\( packet, rest ) -> ( ( packet, rest ), rest ))
                                            result
                                    )
                                    rest'
                    in
                    Just
                        ( Operator version operation (fmap fst packets)
                        , snd $ head $ reverse packets
                        )

                _ ->
                    error "invalid packet!"
decodePacket _ =
    error "invalid packet!"


evaluate :: Packet -> Int
evaluate (Literal _ value) =
    value
evaluate (Operator _ Sum packets) =
    sum $ fmap evaluate packets
evaluate (Operator _ Product packets) =
    product $ fmap evaluate packets
evaluate (Operator _ Minimum packets) =
    minimum $ fmap evaluate packets
evaluate (Operator _ Maximum packets) =
    maximum $ fmap evaluate packets
evaluate (Operator _ GreaterThan [ a, b ]) =
    if evaluate a > evaluate b then 1 else 0
evaluate (Operator _ LessThan [ a, b ]) =
    if evaluate a < evaluate b then 1 else 0
evaluate (Operator _ EqualTo [ a, b ]) =
    if evaluate a == evaluate b then 1 else 0
evaluate _ =
    error "invalid packet!"


decodeValue :: [Bit] -> ( Int, [Bit] )
decodeValue input =
    let
        groups =
            takeWhileInclusive ((==) One . head) (group 5 input)
    in
    ( toDecimal $ concat $ fmap (drop 1) groups
    , drop (5 * length groups) input
    )


group :: Int -> [a] -> [[a]]
group _ [] =
    []
group n list =
    take n list : group n (drop n list)


takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] =
    []
takeWhileInclusive p (x : xs) =
    x
        : if p x then
            takeWhileInclusive p xs

        else
            []


versions :: Packet -> [Int]
versions (Literal version _) =
    [ version ]
versions (Operator version _ packets) =
    version : concat (fmap versions packets)


data Bit
    = Zero
    | One
    deriving (Eq, Show)


parseBits :: String -> [[Bit]]
parseBits [] =
    []
parseBits (c : rest) =
    let
        end =
            parseBits rest

        current =
            case c of
                '0' ->
                    [ Zero, Zero, Zero, Zero ]

                '1' ->
                    [ Zero, Zero, Zero, One ]

                '2' ->
                    [ Zero, Zero, One, Zero ]

                '3' ->
                    [ Zero, Zero, One, One ]

                '4' ->
                    [ Zero, One, Zero, Zero ]

                '5' ->
                    [ Zero, One, Zero, One ]

                '6' ->
                    [ Zero, One, One, Zero ]

                '7' ->
                    [ Zero, One, One, One ]

                '8' ->
                    [ One, Zero, Zero, Zero ]

                '9' ->
                    [ One, Zero, Zero, One ]

                'A' ->
                    [ One, Zero, One, Zero ]

                'B' ->
                    [ One, Zero, One, One ]

                'C' ->
                    [ One, One, Zero, Zero ]

                'D' ->
                    [ One, One, Zero, One ]

                'E' ->
                    [ One, One, One, Zero ]

                'F' ->
                    [ One, One, One, One ]

                _ ->
                    error "invalid hexadecimal!"
    in
    current : end


toDecimal :: [Bit] -> Int
toDecimal =
    sum
        . fmap (uncurry (*))
            . zip powersOf2
                . reverse
                    . fmap bit
    where
        powersOf2 =
            fmap ((^) 2) [0..]

        bit Zero =
            0
        bit One =
            1
