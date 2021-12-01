open Containers

let data = 
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.filter_map int_of_string_opt

let count_increases s =
  OSeq.zip s (OSeq.drop 1 s)
  |> OSeq.filter (fun (x, y) -> x < y)
  |> OSeq.length

let part1 () =
  let datas = OSeq.of_list data in
  Printf.printf "%d\n%!" (count_increases datas)

let part2 () =
  let datas = OSeq.of_list data in
  OSeq.zip (OSeq.zip datas (OSeq.drop 1 datas)) (OSeq.drop 2 datas)
  |> OSeq.map (fun ((x, y), z) -> x + y + z)
  |> count_increases
  |> Printf.printf "%d\n%!"

let () = part2 ()
