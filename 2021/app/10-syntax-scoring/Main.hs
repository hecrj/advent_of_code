module Main where

import Data.Maybe
import Debug.Trace


main :: IO ()
main = do
    lines <- fmap lines getContents

    let
        errors =
            catMaybes $ fmap (either (const Nothing) Just . parse) lines

    putStrLn $ show $ sum $ fmap errorScore errors


data Error
    = Corrupted { expected :: Char, actual :: Char }
    | Incomplete { expected :: Char }
    deriving Show


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
                    Right $ Corrupted (closing c) i

            Left [] ->
                Right $ Incomplete (closing c)

            Right e ->
                Right e
    | otherwise =
        Left (c : rest)


errorScore :: Error -> Int
errorScore (Corrupted _ actual) =
    score actual
errorScore (Incomplete _) =
    0


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


score :: Char -> Int
score ')' =
    3
score ']' =
    57
score '}' =
    1197
score '>' =
    25137
score _ =
    error "invalid character!"


debug :: Show a => a -> a
debug a =
    traceShow a a
