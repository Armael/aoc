open Containers

let data = 
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.filter_map int_of_string_opt

let part1 () =
  let datas = OSeq.of_list data in
  OSeq.product datas datas
  |> OSeq.find (fun (x, y) -> x + y = 2020)
  |> Option.iter (fun (x, y) -> Printf.printf "%d\n%!" (x * y))

let part2 () =
  let datas = OSeq.of_list data in
  OSeq.product3 datas datas datas
  |> OSeq.find (fun (x, y, z) -> x + y + z = 2020)
  |> Option.iter (fun (x, y, z) -> Printf.printf "%d\n%!" (x * y * z))

let () = part2 ()
