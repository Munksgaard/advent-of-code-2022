use "util.sml";

datatype cd_target = Root | Up | Directory of string

datatype command = Ls | Cd of cd_target

fun parseCommand s =
    case words s of
      ["$", "ls"] => Ls
    | ["$", "cd", ".."] => Cd Up
    | ["$", "cd", "/"] => Cd Root
    | ["$", "cd", d] => Cd (Directory d)
    | _ => raise Fail ("Couldn't parse string: " ^ s)

datatype fs_entry = File of int | Dir of (string * fs_entry) list

fun totalSize (File size) = size
  | totalSize (Dir entries) =
      map (totalSize o #2) entries |> sum

fun readLsResult acc [] = (rev acc, [])
  | readLsResult acc (l :: ls) =
      if String.isPrefix "$" l then
          (rev acc, l :: ls)
      else
          case words l of
            ["dir", _] => readLsResult acc ls
          | [size, name] =>
              readLsResult ((name, File (valOf (Int.fromString size))) :: acc)
                  ls
          | _ => raise Fail ("Couldn't parse ls result: " ^ l)

fun toTree ([]: string list) : (string * fs_entry) list * string list = ([], [])
  | toTree (_ :: rest) =
      let
          val (files, rest) = readLsResult [] rest
          fun helper acc [] = (files @ acc, [])
            | helper acc (l :: rest) =
                case parseCommand l of
                  Cd Up => (files @ acc, rest)
                | Cd (Directory d) =>
                    let val (sub_tree, rest) = toTree rest
                    in helper ((d, Dir sub_tree) :: acc) rest
                    end
                | _ => raise Fail ("Unhandled command: " ^ l)
      in
          helper [] rest
      end

fun rootTree (_ :: rest) =
    case toTree rest of
      (t, []) => t
    | _ => raise Fail "rootTree"

fun allSizes total below [] = total :: below
  | allSizes total below ((_, File s) :: rest) =
      allSizes (total + s) below rest
  | allSizes total below ((_, Dir d) :: rest) =
      case allSizes 0 [] d of
        (this :: below') =>
          allSizes (total + this) (this :: below' @ below) rest
      | _ => raise Fail "Empty dir?"

fun solve1 input =
    lines input |> rootTree |> allSizes 0 [] |> List.filter (curry op>= 100000)
        |> sum

fun solve2 input : int =
    let
        val sizes as (total :: _) = lines input |> rootTree |> allSizes 0 []
    in
        sizes |> Listsort.sort Int.compare |> List.filter
            (fn x => total - x <= 40000000) |> hd
    end;

val () =
    let
        val input = trim (TextIO.inputAll (TextIO.stdIn))
        val () = println ("Part 1: " ^ Int.toString (solve1 input))
        val () = println ("Part 2: " ^ Int.toString (solve2 input))
    in
        ()
    end;
