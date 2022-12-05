app "04-camp-cleanup"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.Stderr, pf.Task, pf.File, pf.Path]
    provides [main] to pf

main =
    input <- Task.attempt (File.readUtf8 (Path.fromStr "04-camp-cleanup.in"))

    when input is
        Ok lines ->
            assignments = parse lines

            _ <- assignments
                |> List.keepIf isInefficient
                |> List.len
                |> Num.toStr
                |> Stdout.line
                |> Task.await

            Stdout.line "part 2"

        Err _ ->
            Stderr.line "could not read input"

parse : Str -> List Assignment
parse = \lines ->
    lines
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.map parseAssignment

Assignment : { left : Range, right : Range }

parseAssignment : Str -> Assignment
parseAssignment = \line ->
    when line |> Str.split "," is
        [left, right] -> { left: parseRange left, right: parseRange right }
        _ -> crash "invalid assignment"

isInefficient : Assignment -> Bool
isInefficient = \{ left, right } ->
    contains left right || contains right left

Range : { start : U32, end : U32 }

parseRange : Str -> Range
parseRange = \input ->
    when input |> Str.split "-" is
        [start, end] ->
            {
                start: Str.toU32 start |> unwrap "parse number",
                end: Str.toU32 end |> unwrap "parse number",
            }

        _ -> crash "invalid range"

contains : Range, Range -> Bool
contains = \a, b ->
    a.start <= b.start && a.end >= b.end

unwrap : Result a _, Str -> a
unwrap = \result, expectation ->
    when result is
        Ok a -> a
        Err _ -> crash expectation
