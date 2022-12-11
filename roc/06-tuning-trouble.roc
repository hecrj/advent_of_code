app "06-tuning-trouble"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.Stderr, pf.Task, pf.File, pf.Path]
    provides [main] to pf

main =
    input <- Task.attempt (File.readUtf8 (Path.fromStr "06-tuning-trouble.in"))

    when input is
        Ok lines ->
            buffer = parse lines

            _ <- buffer
                |> startOfPacket
                |> Num.toStr
                |> Stdout.line
                |> Task.await

            buffer
            |> startOfMessage
            |> Num.toStr
            |> Stdout.line

        Err _ ->
            Stderr.line "could not read input"

Buffer : List Str

parse : Str -> Buffer
parse = \lines ->
    lines
    |> Str.trim
    |> Str.graphemes

startOfPacket : Buffer -> Num *
startOfPacket = \buffer ->
    when buffer is
        [a, b, c, d, ..] ->
            if a != b && a != c && a != d && b != c && b != d && c != d then
                4
            else
                1 + startOfPacket (List.dropFirst buffer)

        _ -> crash "no start of packet found!"

startOfMessage : Buffer -> Num a
startOfMessage = \input ->
    startOfMessageHelper : Buffer, Dict Str (Num a), List Str -> Num a
    startOfMessageHelper = \buffer, occurrences, pending ->
        when buffer is
            [a, ..] ->
                newOccurrences =
                    occurrences
                    |> Dict.update
                        a
                        (\value ->
                            when value is
                                Missing -> Present 1
                                Present v -> Present (v + 1)
                        )

                newPending = pending |> List.append a

                finalOccurrences =
                    if List.len newPending > 14 then
                        characterToDiscard = List.first newPending |> unwrap "impossible!"

                        newOccurrences
                        |> Dict.update
                            characterToDiscard
                            (\value ->
                                when value is
                                    Missing -> crash "should not happen!"
                                    Present v -> if v - 1 == 0 then Present 0 else Present (v - 1)
                            )
                    else
                        newOccurrences

                finalPending =
                    if List.len newPending > 14 then
                        List.dropFirst newPending
                    else
                        newPending

                uniqueOccurrences =
                    finalOccurrences
                    |> Dict.walk
                        0
                        (\total, _, n ->
                            if n == 1 then
                                total + 1
                            else
                                total)

                if uniqueOccurrences == 14 then
                    1
                else
                    1 + startOfMessageHelper (List.dropFirst buffer) finalOccurrences finalPending

            [] -> crash "no start of message found!"

    startOfMessageHelper input Dict.empty []

unwrap : Result a _, Str -> a
unwrap = \result, expectation ->
    when result is
        Ok a -> a
        Err _ -> crash expectation
