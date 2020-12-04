open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s -> String.to_list s)

let arr_w = List.length (List.hd data)
let arr_h = List.length data
let arr =
  Array.make_matrix arr_w arr_h '.'

let () =
  List.iteri (fun j line ->
      List.iteri (fun i c ->
          arr.(i).(j) <- c
      ) line
  ) data

let get i j =
  try Some arr.(i mod arr_w).(j) with
    Invalid_argument _ -> None

let part1 (slope_x, slope_y) =
  OSeq.init (fun i -> (slope_x * i, slope_y * i))
  |> OSeq.map (fun (i, j) -> get i j)
  |> OSeq.take_while Option.is_some
  |> OSeq.map Option.get_exn
  |> OSeq.filter (Char.equal '#')
  |> OSeq.length

let part2 () =
  [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]
  |> List.map part1
  |> List.fold_left ( * ) 1

let () = Printf.printf "%d\n%!" (part2 ())
