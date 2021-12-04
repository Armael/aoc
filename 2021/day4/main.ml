open Containers

let numbers, boards =
  let lines = CCIO.(with_in Sys.argv.(1) read_lines_l) in
  let numbers = List.hd lines |> String.split_on_char ',' |> List.map int_of_string in
  let parse_board ls =
    List.tl ls
    |> List.map (fun line ->
      String.split_on_char ' ' line
      |> List.filter (Fun.negate String.is_empty)
      |> List.map (fun s -> (int_of_string s, false))
      |> Array.of_list
    )
    |> Array.of_list
  in
  let boards =
    List.sublists_of_len 6 (List.tl lines)
    |> List.map parse_board
    |> Array.of_list
  in
  numbers, boards

let occurences =
  let tbl = Hashtbl.create 37 in
  Array.iteri (fun board_id board ->
    Array.iteri (fun i row_i ->
      Array.iteri (fun j (x, _) ->
        Hashtbl.add tbl x (board_id, i, j)
      ) row_i
    ) board
  ) boards;
  tbl

let mark x =
  Hashtbl.find_all occurences x |> List.iter (fun (board_id, i, j) ->
    boards.(board_id).(i).(j) <- (x, true)
  )

let check_pos (board_id, i, j) =
  Array.for_all snd boards.(board_id).(i) ||
  List.for_all (fun i -> snd boards.(board_id).(i).(j)) List.(0 --^ 5)

let board_score board_id =
  Array.to_seq boards.(board_id)
  |> Seq.map Array.to_seq
  |> OSeq.flatten
  |> OSeq.filter_map (fun (x, marked) -> if not marked then Some x else None)
  |> OSeq.sum

let part1 () =
  let winning_number, board_id = List.find_map (fun x ->
    mark x;
    List.find_opt check_pos (Hashtbl.find_all occurences x)
    |> Option.map (fun (board_id, _, _) -> x, board_id)
  ) numbers
  |> Option.get_exn_or "oh no"
  in
  Printf.printf "%d\n" (board_score board_id * winning_number)

(* let () = part1 () *)

let part2 () =
  let module ISet = Set.Make(Int) in
  let complete_boards = ref ISet.empty in
  let winning_number, board_id = List.find_map (fun x ->
    let completed_before = !complete_boards in
    mark x;
    let completed =
      Hashtbl.find_all occurences x
      |> List.filter check_pos
      |> List.map (fun (board_id, _, _) -> board_id)
      |> ISet.add_list completed_before
    in
    let newly_completed = ISet.diff completed completed_before in
    complete_boards := completed;
    if ISet.cardinal completed = Array.length boards then (
      assert (ISet.cardinal newly_completed = 1);
      Some (x, ISet.choose newly_completed)
    ) else None
  ) numbers |> Option.get_exn_or "oh no"
  in
  Printf.printf "%d\n" (board_score board_id * winning_number)

let () = part2 ()
