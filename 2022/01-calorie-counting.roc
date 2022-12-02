app "01-calorie-counting"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.Stderr, pf.Task.{attempt, await}, pf.File, pf.Path]
    provides [main] to pf

Calories : U32
Elf : List Calories

main =
    input <- attempt readInput
    when input is
      Ok lines ->
          elves =
              lines
                |> Str.split "\n\n"
                |> List.map (\inventory ->
                    inventory
                      |> Str.split "\n"
                      |> List.map Str.toU32
                      |> List.map (\num -> Result.withDefault num 0)
                )

          _ <- elves
            |> elfWithMostCalories
            |> Num.toStr
            |> Stdout.line
            |> await

          elves
            |> top3Elves
            |> List.sum
            |> Num.toStr
            |> Stdout.line

      Err _ -> Stderr.line "Something went wrong"

elfWithMostCalories : List Elf -> Calories
elfWithMostCalories = \elves ->
    elves
        |> List.map List.sum
        |> List.max
        |> Result.withDefault 0

top3Elves : List Elf -> List Calories
top3Elves = \elves ->
    elves
        |> List.map List.sum
        |> List.sortDesc
        |> List.sublist { start: 0, len: 3 }

readInput =
    File.readUtf8 (Path.fromStr "01-calorie-counting.in")
