open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map String.to_list
  |> List.map (fun l -> List.take 7 l, List.drop 7 l)
  |> List.map (fun (row, col) ->
        List.map (function 'F' -> `L | _ -> `R) row,
        List.map (function 'L' -> `L | _ -> `R) col
     )

let rec interp i j = function
  | [] -> (i, j)
  | `L :: l -> interp i (i + (j - i) / 2) l
  | `R :: l -> interp (i + (j - i) / 2) j l

let seat_ids () =
  List.map (fun (row, col) ->
    let (row, row_next) = interp 0 128 row in
    let (col, col_next) = interp 0 8 col in
    assert (row + 1 = row_next);
    assert (col + 1 = col_next);
    8 * row + col
  ) data

let part1 () =
  List.to_std_seq (seat_ids ())
  |> OSeq.max ~lt:(<)

let part2 () =
  let ids = List.sort compare (seat_ids ()) in
  OSeq.zip (OSeq.of_list ids) (OSeq.of_list (List.tl ids))
  |> OSeq.find (fun (id, id') -> id + 2 = id')
  |> Option.get_exn
  |> fst |> succ

let () = Printf.printf "%d\n%!" (part2 ())
