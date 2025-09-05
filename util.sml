load "Hashset";
load "Binarymap";
load "Int";
load "TextIO";
load "String";
load "Listsort";
load "Substring";

fun uncurry f (x, y) = f x y

fun curry f x y = f (x, y)

fun flip f x y = f y x

val lines = String.fields (fn c => c = #"\n");

val words = String.fields Char.isSpace;

val lines' = String.tokens (fn c => c = #"\n");

val words' = String.tokens Char.isSpace;

fun println s = (print s; print "\n");

fun maximum [] = raise Option
  | maximum (x :: xs) = foldl Int.max x xs

val sum = foldl op+ 0

infix contains

fun [] contains y = false
  | (x :: xs) contains y = x = y orelse xs contains y

fun delete (x : ''a) : ''a list -> ''a list  =
    let fun aux acc [] = rev acc
          | aux acc (y :: ys) =
            if x = y then aux acc ys else aux (y :: acc) ys
    in aux []
    end

fun id x = x

val iota = flip (curry List.tabulate) id

fun difference xs ys = foldl (uncurry delete) xs ys

fun intersection xs = rev o foldl (fn (y, acc) => if xs contains y then y :: acc else acc) []

infix |>
fun x |> f = f x

val trim = Substring.string o
           Substring.dropr Char.isSpace o
           Substring.dropl Char.isSpace o
           Substring.full

val trimr = Substring.string o
            Substring.dropr Char.isSpace o
            Substring.full


fun breakIndex i xs = (List.take (xs, i), List.drop (xs, i))

fun breakBy f =
    let fun aux acc [] = (rev acc, [])
          | aux acc (y :: ys) = if f y then
                                    (rev acc, y :: ys)
                                else
                                    aux (y :: acc) ys
    in aux []
    end

val break = breakBy o curry op=

(* The exclusive version, the splitter is not included in the result *)
fun breakExclusive x ys =
    let val (first, rest) = break x ys
    in (first, tl rest)
    end

fun groupCount i =
    let fun aux acc [] = rev acc
          | aux acc xs =
            let val (first, rest) = breakIndex i xs
            in aux (first :: acc) rest
            end
    in aux []
    end

fun foldl1 f (x :: xs) = foldl f x xs
  | foldl1 _ _ = raise Empty

fun last xs = hd (List.drop (xs, List.length xs - 1))

fun const x _ = x

fun modify f i xs =
    let val first = List.take (xs, i)
    in case List.drop (xs, i) of
           x :: rest => first @ [f x] @ rest
         | _ => raise Empty
    end

fun update y = modify (const y)

fun dedup xs =
    let fun aux (x :: xs) (y :: ys) =
            if x = y then aux (x :: xs) ys else aux (y :: x :: xs) ys
          | aux [] (y :: ys) = aux [y] ys
          | aux acc [] = rev acc
    in aux [] xs end

fun uniqueBy f =
    dedup o Listsort.sort f

val unique =
    uniqueBy Char.compare

fun vtranspose v =
    Vector.tabulate (Vector.length (Vector.sub (v, 0)),
                     fn i => Vector.tabulate (Vector.length v,
                                              fn j => Vector.sub (Vector.sub (v, j), i)))
fun pair x y = (x,y)

fun zipWith f =
    let fun aux acc [] _ = rev acc
          | aux acc _ [] = rev acc
          | aux acc (x :: xs) (y :: ys) = aux (f x y :: acc) xs ys
    in aux []
    end

fun zip x y = zipWith pair x y

fun splitBy f =
    let fun aux acc [] = rev acc
          | aux acc xs =
            case breakBy f xs of
                (l, []) => rev (l :: acc)
              | (l, _ :: r) => aux (l :: acc) r
    in aux []
    end

val split = splitBy o curry op=
