open Containers
open CCFun
let (=) = Stdlib.(=)

let priority c =
  match c with
  | 'a'..'z' -> Char.code c - Char.code 'a' + 1
  | 'A'..'Z' -> Char.code c - Char.code 'A' + 27
  | _ -> assert false

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)

module CSet = Set.Make (Char)

let part1 () =
  data
  |> List.map (fun s ->
    String.sub s 0 (String.length s / 2),
    String.sub s (String.length s / 2) (String.length s / 2))
  |> List.map (Pair.map_same (String.to_iter %> CSet.of_iter))
  |> List.map (fun (s1, s2) -> CSet.choose (CSet.inter s1 s2))
  |> List.map priority
  |> List.fold_left (+) 0
  |> Printf.printf "%d\n"

let part2 () =
  OSeq.of_list data
  |> OSeq.zip_index
  |> OSeq.group ~eq:(fun (i,_) (j,_) -> i/3 = j/3)
  |> OSeq.map (OSeq.map (snd %> String.to_seq %> CSet.of_seq))
  |> OSeq.map (OSeq.reduce CSet.inter)
  |> OSeq.map (CSet.choose %> priority)
  |> OSeq.fold_left (+) 0
  |> Printf.printf "%d\n"

let () =
  part2 ()
