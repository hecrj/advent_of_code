app "02-rock-paper-scissors"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.Stderr, pf.Task.{ Task, attempt, await }, pf.File, pf.Path]
    provides [main] to pf

main =
    input <- attempt (File.readUtf8 (Path.fromStr "02-rock-paper-scissors.in"))

    when input is
        Ok lines ->
            rounds = parse lines parseRound
            actualRounds = parse lines parseActualRound

            _ <- await
                    (
                        when rounds is
                            Ok r ->
                                r
                                |> List.map score
                                |> List.sum
                                |> Num.toStr
                                |> Stdout.line

                            Err error ->
                                Stderr.line
                                    (
                                        when error is
                                            InvalidRound -> "invalid round"
                                            InvalidShape -> "invalid shape"
                                            _ -> "something went wrong"
                                    )
                    )

            when actualRounds is
                Ok r ->
                    r
                    |> List.map actualScore
                    |> List.sum
                    |> Num.toStr
                    |> Stdout.line

                Err error ->
                    Stderr.line
                        (
                            when error is
                                InvalidRound -> "invalid round"
                                InvalidShape -> "invalid shape"
                                _ -> "something went wrong"
                        )

        Err _ ->
            Stderr.line "could not read input"

parse : Str, (Str -> Result a err) -> Result (List a) err
parse = \lines, parser ->
    lines
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.mapTry parser

Round : { opponent : Shape, me : Shape }

parseRound : Str -> Result Round _
parseRound = \line ->
    when Str.split line " " is
        [a, b] ->
            parseOpponentShape a
            |> Result.try
                (\opponent ->
                    parseMyShape b
                    |> Result.try (\me -> Ok { opponent, me })
                )

        _ ->
            Err InvalidRound

score : Round -> Num *
score = \round ->
    shapeScore round.me + outcomeScore (roundOutcome round)

roundOutcome : Round -> Outcome
roundOutcome = \{ opponent, me } ->
    if opponent == me then
        Draw
    else if wins me opponent then
        Win
    else
        Loss

Shape : [Rock, Paper, Scissors]

parseOpponentShape : Str -> Result Shape _
parseOpponentShape = \char ->
    when char is
        "A" -> Ok Rock
        "B" -> Ok Paper
        "C" -> Ok Scissors
        _ -> Err InvalidShape

parseMyShape : Str -> Result Shape _
parseMyShape = \char ->
    when char is
        "X" -> Ok Rock
        "Y" -> Ok Paper
        "Z" -> Ok Scissors
        _ -> Err InvalidShape

shapeScore : Shape -> Num *
shapeScore = \shape ->
    when shape is
        Rock -> 1
        Paper -> 2
        Scissors -> 3

wins : Shape, Shape -> Bool
wins = \a, b ->
    when [a, b] is
        [Rock, Scissors] -> Bool.true
        [Paper, Rock] -> Bool.true
        [Scissors, Paper] -> Bool.true
        _ -> Bool.false

Outcome : [Win, Draw, Loss]

parseOutcome : Str -> Result Outcome _
parseOutcome = \line ->
    when line is
        "X" -> Ok Loss
        "Y" -> Ok Draw
        "Z" -> Ok Win
        _ -> Err InvalidOutcome

outcomeScore : Outcome -> Num *
outcomeScore = \o ->
    when o is
        Win -> 6
        Draw -> 3
        Loss -> 0

ActualRound : { opponent : Shape, outcome : Outcome }

parseActualRound : Str -> Result ActualRound _
parseActualRound = \line ->
    when Str.split line " " is
        [a, b] ->
            parseOpponentShape a
            |> Result.try
                (\opponent ->
                    parseOutcome b
                    |> Result.try (\outcome -> Ok { opponent, outcome })
                )

        _ ->
            Err InvalidRound

actualScore : ActualRound -> Num *
actualScore = \round ->
    outcomeScore round.outcome + shapeScore (findMyShape round)

findMyShape : ActualRound -> Shape
findMyShape = \round ->
    when round.outcome is
        Draw -> round.opponent
        Win ->
            when round.opponent is
                Rock -> Paper
                Paper -> Scissors
                Scissors -> Rock

        Loss ->
            when round.opponent is
                Rock -> Scissors
                Paper -> Rock
                Scissors -> Paper
