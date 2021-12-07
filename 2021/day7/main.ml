open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_all)
  |> String.trim
  |> String.split_on_char ','
  |> List.map int_of_string

let maxpos = OSeq.max ~lt:(<) (OSeq.of_list data)

let part1 () =
  let cost = Array.make (maxpos + 1) 0 in
  List.iter (fun crab ->
    for i = 0 to maxpos do
      cost.(i) <- cost.(i) + abs (crab - i)
    done
  ) data;
  OSeq.of_array cost
  |> OSeq.max ~lt:(>)
  |> Printf.printf "%d\n"

(* let () = part1 () *)

let part2 () =
  let cost = Array.make (maxpos + 1) 0 in
  List.iter (fun crab ->
    for i = 0 to maxpos do
      let d = abs (crab - i) in
      cost.(i) <- cost.(i) + d * (d + 1) / 2
    done
  ) data;
  OSeq.of_array cost
  |> OSeq.max ~lt:(>)
  |> Printf.printf "%d\n"

let () = part2 ()
