use "util.sml";

(* 30373 *)
(* 25512 *)
(* 65332 *)
(* 33549 *)
(* 35390 *)


val test = "30373\n25512\n65332\n33549\n35390"

fun visibleTrees (x, []) = [x]
  | visibleTrees (x as (height : char, _), acc as ((current_max, _) :: _)) =
    if height > current_max then x :: acc else acc

fun toMatrix xs =
    Vector.fromList (map Vector.fromList xs)

fun solve1 input =
    let val m = toMatrix (splitBy (fn (c, _) => c = #"\n")
                                  (zip (explode input)
                                       (iota (size input))))
        val m' = vtranspose m
    in List.concat [Vector.foldl (fn (row, acc) => Vector.foldl visibleTrees [] row @ acc) [] m,
                    Vector.foldl (fn (row, acc) => Vector.foldr visibleTrees [] row @ acc) [] m,
                    Vector.foldl (fn (row, acc) => Vector.foldl visibleTrees [] row @ acc) [] m',
                    Vector.foldl (fn (row, acc) => Vector.foldr visibleTrees [] row @ acc) [] m']
                   |> uniqueBy (fn ((_, i), (_, j)) => Int.compare (i, j))
                   |> length
    end

fun sightLength count tree [] = (count, [(count + 1, tree)])
  | sightLength count tree ((num, height) :: rest) =
    if tree >= height then
        sightLength (count + num) tree rest
    else
        (count + 1, (count + 1, tree) :: (num, height) :: rest)

val sightScore =
    let fun sightScoreHelper (tree, (scoreList, acc)) =
            let val (score, scoreList') = sightLength 0 tree scoreList
            in (scoreList', score :: acc)
            end
    in rev o #2 o Vector.foldl sightScoreHelper ([], [])
    end

fun bruteit f (i, j) height (vs : char vector vector) =
    let val x = Vector.sub (Vector.sub (vs, i), j)
    in if x < height then
           1 + bruteit f (f (i, j)) height vs
       else
           1
    end
    handle Subscript => 0

fun goUp (i, j) = (i - 1, j)

fun goDown (i, j) = (i + 1, j)

fun goLeft (i, j) = (i, j - 1)

fun goRight (i, j) = (i, j + 1)

fun solve2 input : int =
    let val m = toMatrix (splitBy (fn c => c = #"\n") (explode input))
        val m' = vtranspose m
    in raise Fail "hej"
    end

;

val () =
    let val input = trim (TextIO.inputAll (TextIO.stdIn))
        val () = println ("Part 1: " ^ Int.toString (solve1 input))
        val () = println ("Part 2: " ^ Int.toString (solve2 input))
    in ()
    end;
