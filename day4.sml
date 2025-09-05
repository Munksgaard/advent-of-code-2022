use "util.sml";

fun toRange xs =
    let val (first, _ :: rest) = break #"-" xs
    in (valOf (Int.fromString (implode first)), valOf (Int.fromString (implode rest)))
    end

fun isFullyContained (x, y) (x', y') =
    x >= x' andalso y <= y'

fun overlaps (x, y) (x', y') =
    x >= x' andalso x <= y' orelse y >= x' andalso y <= y'

fun filterLine f (first, rest) =
    let val first = toRange first
        val rest = toRange rest
    in f first rest orelse f rest first
    end

fun solve1 input =
    lines input
          |> map (breakExclusive #"," o explode)
          |> List.filter (filterLine isFullyContained)
          |> length

fun solve2 input : int =
    lines input
          |> map (breakExclusive #"," o explode)
          |> List.filter (filterLine overlaps)
          |> length

;

val () =
    let val input = trim (TextIO.inputAll (TextIO.stdIn))
        val () = println ("Part 1: " ^ Int.toString (solve1 input))
        val () = println ("Part 2: " ^ Int.toString (solve2 input))
    in ()
    end;
