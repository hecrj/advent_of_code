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

            Stdout.line "part 2"

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
