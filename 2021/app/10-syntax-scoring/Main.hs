{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Data.Maybe
import Debug.Trace
import qualified Data.List as List


main :: IO ()
main = do
    lines <- fmap lines getContents

    let
        results =
            zip lines $ fmap parse lines

        errors f =
            catMaybes $
                fmap
                    (\( input, result ) ->
                        fmap ((,) input) $ either (const Nothing) Just result >>= f
                    )
                    results

        corruptedErrors =
            errors corrupted

        incompleteErrors =
            errors incomplete

    putStrLn $ show $ sum $ fmap (corruptedScore . snd) corruptedErrors
    putStrLn $ show $ middle $ List.sort $ fmap (uncurry incompleteScore) incompleteErrors


parse :: String -> Either String Error
parse [] =
    Left []
parse (c : rest)
    | isOpening c =
        case parse rest of
            Left (i : rest) ->
                if i == closing c then
                    parse rest

                else
                    Right $ CorruptedError $ Corrupted (closing c) i

            Left [] ->
                Right $ IncompleteError $ Incomplete (closing c)

            Right e ->
                Right e
    | otherwise =
        Left (c : rest)


isOpening :: Char -> Bool
isOpening '(' =
    True
isOpening '[' =
    True
isOpening '{' =
    True
isOpening '<' =
    True
isOpening _ =
    False


closing :: Char -> Char
closing '(' =
    ')'
closing '[' =
    ']'
closing '{' =
    '}'
closing '<' =
    '>'
closing _ =
    error "invalid character!"


data Error
    = CorruptedError Corrupted
    | IncompleteError Incomplete
    deriving Show


corrupted :: Error -> Maybe Corrupted
corrupted (CorruptedError c) =
    Just c
corrupted (IncompleteError _) =
    Nothing


incomplete :: Error -> Maybe Incomplete
incomplete (CorruptedError _) =
    Nothing
incomplete (IncompleteError i) =
    Just i


data Corrupted
    = Corrupted { expected :: Char, actual :: Char }
    deriving Show


corruptedScore :: Corrupted -> Int
corruptedScore (Corrupted _ actual) =
    case actual of
        ')' ->
            3

        ']' ->
            57

        '}' ->
            1197

        '>' ->
            25137

        _ ->
            error "invalid character!"


data Incomplete
    = Incomplete { expected :: Char }
    deriving Show


incompleteScore :: String -> Incomplete -> Int
incompleteScore input incomplete =
    foldl (\total c -> total * 5 + score c) 0 $
        missingCharacters input incomplete
    where
        score ')' =
            1
        score ']' =
            2
        score '}' =
            3
        score '>' =
            4
        score _ =
            error "invalid character!"


missingCharacters :: String -> Incomplete -> String
missingCharacters input (Incomplete expected) =
    let
        candidate =
            input ++ [ expected ]
    in
    case parse candidate of
        Left [] ->
            [ expected ]

        Right (IncompleteError i) ->
            (expected : missingCharacters candidate i)

        Left _ ->
            error "unexpected leftover input!"

        Right _ ->
            error "unexpected error!"


debug :: Show a => a -> a
debug a =
    traceShow a a


middle :: [a] -> a
middle list =
    list !! (length list `div` 2)
