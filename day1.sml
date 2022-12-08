use "util.sml";

fun pushFirst l [] = [[l]]
  | pushFirst l ([] :: rest) = [l] :: rest
  | pushFirst l (xs :: rest) = (l :: xs) :: rest

fun groupItems acc [] = acc
  | groupItems acc ("" :: ls) = groupItems ([] :: acc) ls
  | groupItems acc (l :: ls) = groupItems (pushFirst l acc) ls

fun solve_1 input =
    lines input
          |> groupItems []
          |> map (sum o map (valOf o Int.fromString))
          |> maximum

fun solve_2 input : int =
    lines input
          |> groupItems []
          |> map (sum o map (valOf o Int.fromString))
          |> Listsort.sort Int.compare
          |> rev
          |> flip (curry List.take) 3
          |> sum

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
        val () = println ("Part 1: " ^ Int.toString (solve_1 input))
        val () = println ("Part 2: " ^ Int.toString (solve_2 input))
    in ()
    end;
