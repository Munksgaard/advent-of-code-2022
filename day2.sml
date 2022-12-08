use "util.sml";

fun outcome you me = me + (if you = me then 3 else if me = you mod 3 + 1 then 6 else 0)

fun scoreRound [you, me] =
    let val you = ord you - ord #"A" + 1
        val me = ord me - ord #"X" + 1
    in outcome you me end
  | scoreRound _ = raise Option

fun loseTo 1 = 3
  | loseTo 2 = 1
  | loseTo 3 = 2
  | loseTo _ = raise Option

fun winTo 1 = 2
  | winTo 2 = 3
  | winTo 3 = 1
  | winTo _ = raise Option

fun scoreRound2 [you, desired] =
    let val you = ord you - ord #"A" + 1
    in case desired of
           #"X" => loseTo you + 0
         | #"Y" => you + 3
         | #"Z" => winTo you + 6
         | _ => raise Option
    end
  | scoreRound2 _ = raise Option

fun solve_1 input =
    lines input
          |> map (map (hd o explode) o words)
          |> map scoreRound
          |> sum

fun solve_2 input : int =
    lines input
          |> map (map (hd o explode) o words)
          |> map scoreRound2
          |> sum

;

val () =
    let val input = TextIO.inputAll (TextIO.stdIn)
        val () = println ("Part 1: " ^ Int.toString (solve_1 input))
        val () = println ("Part 2: " ^ Int.toString (solve_2 input))
    in ()
    end;
