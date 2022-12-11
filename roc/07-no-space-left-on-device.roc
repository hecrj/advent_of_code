app "07-no-space-left-on-device"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.Stderr, pf.Task, pf.File, pf.Path]
    provides [main] to pf

main =
    input <- Task.attempt (File.readUtf8 (Path.fromStr "07-no-space-left-on-device.in"))

    when input is
        Ok lines ->
            root = parse lines

            _ <- root
                |> directorySizes
                |> List.map .size
                |> List.keepIf (\size -> size >= 100000)
                |> List.sum
                |> Num.toStr
                |> Stdout.line
                |> Task.await

            Stdout.line "part 2"

        Err _ ->
            Stderr.line "could not read input"

Entry : [Directory { name : Str, entries : List Entry }, File { name : Str, size : U32 }]

parse : Str -> Directory
parse = \lines ->
    lines
    |> Str.split "$"
    |> List.map Str.trim
    |> List.dropIf Str.isEmpty
    |> List.dropFirst # First one is always "cd /"
    |> List.map parseCommand
    |> run { name: "/", entries: [] }

Command : [Cd Target, Ls (List File)]
Target : [Parent, Child Str]

parseCommand : Str -> Command
parseCommand = \input ->
    lines = input |> Str.split "\n"

    when lines is
        [command] ->
            when command |> Str.split " " is
                ["cd", ".."] -> Cd Parent
                ["cd", name] -> Cd (Child (Str.trim name))
                _ -> crash "invalid no output command!"

        ["ls", ..] ->
            output = List.dropFirst lines

            Ls
                (
                    output
                    |> List.dropIf (\entry -> Str.startsWith entry "dir")
                    |> List.map parseFile
                )

        _ -> crash "invalid command!"

run : List Command, Directory -> Directory
run = \commands, currentDir ->
    currentDir

Directory : { name : Str, entries : List Entry }

directorySizes : Directory -> List { dir : Directory, size : U32 }
directorySizes = \dir -> [{ dir, size: 0 }]

File : { name : Str, size : U32 }

parseFile : Str -> File
parseFile = \input ->
    when input |> Str.split " " is
        [size, name] -> { name, size: Str.toU32 size |> unwrap "invalid size!" }
        _ -> crash "invalid file!"

unwrap : Result a _, Str -> a
unwrap = \result, expectation ->
    when result is
        Ok a -> a
        Err _ -> crash expectation
