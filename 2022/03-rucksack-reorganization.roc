app "03-rucksack-reorganization"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.Stderr, pf.Task, pf.File, pf.Path]
    provides [main] to pf

main =
    input <- Task.attempt (File.readUtf8 (Path.fromStr "03-rucksack-reorganization.in"))

    when input is
        Ok lines ->
            rucksacks = parse lines

            _ <- rucksacks
                |> List.map repeatedItem
                |> List.map itemScore
                |> List.sum
                |> Num.toStr
                |> Stdout.line
                |> Task.await

            rucksacks
            |> groups
            |> List.map groupItem
            |> List.map itemScore
            |> List.sum
            |> Num.toStr
            |> Stdout.line

        Err _ ->
            Stderr.line "could not read input"

parse : Str -> List Rucksack
parse = \lines ->
    lines
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.map parseRucksack

groups : List Rucksack -> List Group
groups = \rucksacks ->
    when rucksacks is
        [a, b, c, ..] ->
            rucksacks
            |> List.sublist { start: 3, len: List.len rucksacks - 3 }
            |> groups
            |> List.prepend { a, b, c }

        _ -> []

Group : { a : Rucksack, b : Rucksack, c : Rucksack }

groupItem : Group -> Item
groupItem = \{ a, b, c } ->
    repeatedAB = repeatedItems (toItems a) (toItems b)
    repeatedABC = repeatedItems (repeatedAB) (toItems c)

    repeatedABC
    |> List.first
    |> Result.withDefault ""

Rucksack : { left : Compartment, right : Compartment }

parseRucksack : Str -> Rucksack
parseRucksack = \items ->
    graphemes = Str.graphemes items
    compartmentSize = List.len graphemes // 2

    {
        left: List.sublist graphemes { start: 0, len: compartmentSize },
        right: List.sublist graphemes { start: compartmentSize, len: compartmentSize },
    }

repeatedItem : Rucksack -> Item
repeatedItem = \{ left, right } ->
    left
    |> repeatedItems right
    |> List.first
    |> Result.withDefault ""

toItems : Rucksack -> List Item
toItems = \{ left, right } -> List.concat right left

Compartment : List Item

Item : Str

itemScore : Str -> U32
itemScore = \item ->
    lowercaseStartCodePoint = Str.toScalars "a" |> List.first |> Result.withDefault 0
    uppercaseStartCodePoint = Str.toScalars "A" |> List.first |> Result.withDefault 0
    itemCodePoint = Str.toScalars item |> List.first |> Result.withDefault 0

    if itemCodePoint < lowercaseStartCodePoint then
        itemCodePoint - uppercaseStartCodePoint + 27
    else
        itemCodePoint - lowercaseStartCodePoint + 1

repeatedItems : List Item, List Item -> List Item
repeatedItems = \a, b ->
    a |> List.keepIf (\i -> List.contains b i)
