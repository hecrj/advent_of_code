app "02-rock-paper-scissors"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.Stderr, pf.Task.{Task, attempt}, pf.File, pf.Path]
    provides [main] to pf


main =
    result <- attempt parse
    when result is
      Ok rounds ->
        rounds
            |> List.map score
            |> List.sum
            |> Num.toStr
            |> Stdout.line

      Err error -> 
          Stderr.line (
              when error is
                  InvalidRound -> "invalid round"
                  InvalidShape -> "invalid shape"
                  _ -> "something went wrong"
          )


parse : Task (List Round) _
parse =
    input <- attempt (File.readUtf8 (Path.fromStr "02-rock-paper-scissors.in"))

    Result.try input (\lines ->
        lines
            |> Str.split "\n"
            |> List.dropIf Str.isEmpty
            |> List.mapTry parseRound
    )
    |> Task.fromResult

Round : { opponent : Shape, me: Shape }

parseRound : Str -> Result Round _
parseRound = \line ->
    when Str.split line " " is
      [a, b] ->
            parseOpponentShape a
              |> Result.try (\opponent ->
                parseMyShape b
                  |> Result.try (\me -> Ok { opponent, me })
              )

      _ ->
          Err InvalidRound

score : Round -> Num *
score = \round ->
    shapeScore round.me + outcomeScore (outcome round)

outcome : Round -> Outcome
outcome = \{ opponent, me } ->
    if opponent == me then
        Draw
    else if wins me opponent then
        Win
    else
        Loss

Shape : [ Rock, Paper, Scissors ]

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

outcomeScore : Outcome -> Num *
outcomeScore = \o ->
    when o is
        Win -> 6
        Draw -> 3
        Loss -> 0
