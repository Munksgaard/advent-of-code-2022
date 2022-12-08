use "util.sml";

fun toStack desc =
    let val max_height = length desc - 1
        val num_stacks = valOf (Int.fromString (last (words' (last desc))))
        val desc = map explode desc
        fun makeStack acc i stack =
            if i < max_height then
                let val c = List.nth (List.nth (desc, max_height - i - 1), stack * 4 + 1)
                in if c = #" " then
                       acc
                   else
                       makeStack (c :: acc) (i + 1) stack
                end
            else
                acc
    in List.tabulate (num_stacks, makeStack [] 0)
    end

fun executeMove f (move, stack) =
    let val count = valOf (Int.fromString (List.nth (move, 1)))
        val from = valOf (Int.fromString (List.nth (move, 3))) - 1
        val to = valOf (Int.fromString (List.nth (move, 5))) - 1
        val stack =  modify (curry op@ (f (List.take (List.nth (stack, from), count)))) to stack
                            |> modify (flip (curry List.drop) count) from
    in stack
    end

fun solver f input =
    let val (stack_description, instructions) = splitAt' "" (lines input)
        val stack = toStack stack_description
    in implode (map hd (foldl (executeMove f) stack (map words instructions)))
    end

;

val () =
    let val input = trimr (TextIO.inputAll (TextIO.stdIn))
        val () = println ("Part 1: " ^ solver rev input)
        val () = println ("Part 2: " ^ solver id input)
    in ()
    end;
