open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.filter_map int_of_string_opt
  |> Array.of_list

let check_n n =
  let x = data.(n) in
  let before = OSeq.((n - 25) --^ n) in
  Option.is_some @@
  begin OSeq.product before before
    |> OSeq.filter (fun (i, j) -> i <> j)
    |> OSeq.find (fun (i, j) -> data.(i) + data.(j) = x)
  end

let part1 () =
  OSeq.iterate 25 succ
  |> OSeq.find (fun n -> not (check_n n))
  |> Option.get_exn
  |> Array.get data

let part2 () =
  let target = part1 () in
  OSeq.iterate 0 succ
  |> OSeq.find_map (fun i ->
    OSeq.iterate i succ
    |> OSeq.map (fun j -> (j, data.(j)))
    |> OSeq.fold_while (fun (sum, k) (j, x) ->
         if sum >= target then (sum, k), `Stop
         else (sum + x, j), `Continue
       ) (0, -1)
    |> fun (sum, k) -> if sum = target then Some (i, k) else None
  )
  |> Option.map (fun (i, k) ->
         let vs = OSeq.(i -- k) |> OSeq.map (Array.get data) in
         let min = OSeq.min ~lt:(<) vs in
         let max = OSeq.max ~lt:(<) vs in
         min + max)

let () =
  let Some x = part2 () in
  Printf.printf "%d\n%!" x
