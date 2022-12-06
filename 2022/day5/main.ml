open Containers
open CCFun
let (=) = Stdlib.(=)
let (<>) = Stdlib.(<>)

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)

let crates_data, moves_data =
  let c, m = List.take_drop_while (fun s -> s <> "") data in
  List.take (List.length c - 1) c, List.tl m

let crates () =
  OSeq.of_list crates_data
  |> OSeq.map (fun s ->
    String.to_seq s
    |> OSeq.zip_index
    |> OSeq.filter (fun (i, _) -> i mod 4 = 1)
    |> OSeq.map (fun (_, c) -> if c = ' ' then None else Some c))
  |> OSeq.transpose
  |> OSeq.map (OSeq.to_list %> List.filter_map Fun.id)
  |> OSeq.to_array

let moves =
  List.map (fun s -> Scanf.sscanf s "move %d from %d to %d" (fun n from to_ -> (n, from-1, to_-1)))
    moves_data

let eval_move is_rev crates (n, from, to_) =
  let top, rest = List.take_drop n crates.(from) in
  crates.(to_) <- (if is_rev then List.rev top else top) @ crates.(to_);
  crates.(from) <- rest

let go is_rev =
  let crates = crates () in
  List.iter (eval_move is_rev crates) moves;
  Array.to_list crates |> List.map List.hd |> String.of_list |> print_endline

(* part1: *)
(* let () = go true *)
let () = go false
