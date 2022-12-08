use "util.sml";

fun charToPriority c =
    if Char.isLower c then
        ord c - ord #"a" + 1
    else
        ord c - ord #"A" + 27

fun linePriority xs =
    let val l = length xs
        val first = List.take (xs, l div 2)
        val last = List.drop (xs, l div 2)
    in charToPriority (hd (intersection first last))
    end

fun solve1 input =
    lines input
          |> map (linePriority o explode)
          |> sum

fun solve2 input : int =
    lines input
          |> map explode
          |> groupCount 3
          |> map (hd o map charToPriority o foldl1 (uncurry intersection))
          |> sum

;

val () =
    let val input = trim (TextIO.inputAll (TextIO.stdIn))
        val () = println ("Part 1: " ^ Int.toString (solve1 input))
        val () = println ("Part 2: " ^ Int.toString (solve2 input))
    in ()
    end;
