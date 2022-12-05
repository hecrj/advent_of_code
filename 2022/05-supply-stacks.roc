app "05-supply-stacks"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.Stderr, pf.Task, pf.File, pf.Path]
    provides [main] to pf

main =
    input <- Task.attempt (File.readUtf8 (Path.fromStr "05-supply-stacks.in"))

    when input is
        Ok lines ->
            program = parse lines

            _ <- program
                |> run
                |> topCrates
                |> Stdout.line
                |> Task.await

            program
            |> run9001
            |> topCrates
            |> Stdout.line

        Err _ ->
            Stderr.line "could not read input"

Program : { stacks : List Stack, instructions : List Instruction }

parse : Str -> Program
parse = \lines ->
    when lines |> Str.split "\n\n" is
        [stacks, instructions] ->
            parsedStacks =
                stacks
                |> Str.split "\n"
                |> List.dropIf Str.isEmpty
                |> List.map Str.graphemes
                |> transpose
                |> List.dropIf (\column -> (column |> List.last |> unwrap "empty column") == " ")
                |> List.map
                    (\list -> list
                        |> List.dropIf (\c -> c == " ")
                        |> List.dropLast
                        |> List.reverse)

            parsedInstructions =
                instructions
                |> Str.split "\n"
                |> List.dropIf Str.isEmpty
                |> List.map parseInstruction

            { stacks: parsedStacks, instructions: parsedInstructions }

        _ -> crash "invalid input"

run : Program -> List Stack
run = \program -> runHelper program List.reverse

run9001 : Program -> List Stack
run9001 = \program -> runHelper program (\x -> x)

runHelper : Program, (List Str -> List Str) -> List Stack
runHelper = \{ stacks, instructions }, moveMap ->
    when instructions is
        [] -> stacks
        [instruction, ..] ->
            from = List.get stacks (instruction.from - 1) |> unwrap "invalid from"
            to = List.get stacks (instruction.to - 1) |> unwrap "invalid to"

            cratesToMove = List.takeLast from instruction.amount

            newFrom = List.sublist from {
                start: 0,
                len: List.len from - instruction.amount,
            }

            # Weird bug if I don't recreate the list here
            newTo = List.concat (List.walk to [] List.append) (moveMap cratesToMove)

            newStacks =
                stacks
                |> List.set (instruction.from - 1) newFrom
                |> List.set (instruction.to - 1) newTo

            runHelper { stacks: newStacks, instructions: List.dropFirst instructions } moveMap

topCrates : List Stack -> Str
topCrates = \stacks ->
    stacks
    |> List.map (\stack -> List.last stack |> Result.withDefault " ")
    |> List.walk "" Str.concat

Stack : List Str

Instruction : { amount : Nat, from : Nat, to : Nat }

parseInstruction : Str -> Instruction
parseInstruction = \input ->
    when Str.split input " " is
        ["move", amount, "from", from, "to", to] ->
            {
                amount: Str.toNat amount |> unwrap "invalid amount",
                from: Str.toNat from |> unwrap "invalid from",
                to: Str.toNat to |> unwrap "invalid to",
            }

        _ -> crash "invalid instruction"

transpose : List (List a) -> List (List a)
transpose = \rows ->
    when rows is
        [] -> []
        [[], ..] -> []
        _ ->
            rows
            |> List.map List.dropFirst
            |> transpose
            |> List.prepend
                (
                    rows
                    |> List.map (\row -> row |> List.first |> unwrap "empty row")
                )

unwrap : Result a _, Str -> a
unwrap = \result, expectation ->
    when result is
        Ok a -> a
        Err _ -> crash expectation
