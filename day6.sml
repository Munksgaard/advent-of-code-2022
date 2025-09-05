use "util.sml";

fun solve1 i (a :: b :: c :: rest) (x :: xs) =
    if length (unique [a,b,c,x]) <> 4 then
        solve1 (i + 1) (x :: a :: b :: c :: rest) xs
    else
        i
  | solve1 i acc (x :: xs) = solve1 (i + 1) (x :: acc) xs
  | solve1 _ _ _ = raise Empty

fun solve2 _ _ [] = raise Empty
  | solve2 i acc (x :: xs) =
    let val last = List.take (acc, 13)
    in if length (unique (x :: last)) <> 14 then
           solve2 (i + 1) (x :: acc) xs
       else
           i
    end
    handle Subscript => solve2 (i + 1) (x :: acc) xs

;

val () =
    let val input = trim (TextIO.inputAll (TextIO.stdIn))
        val () = println ("Part 1: " ^ Int.toString (solve1 1 [] (explode input)))
        val () = println ("Part 2: " ^ Int.toString (solve2 1 [] (explode input)))
    in ()
    end;
