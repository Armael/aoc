open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map int_of_string_opt
  |> List.group_succ ~eq:(fun o1 o2 -> Bool.equal (Option.is_none o1) (Option.is_none o2))
  |> List.filter (fun l -> not (Stdlib.(=) l [None]))
  |> List.map (List.map @@ Option.get_exn_or "kesk")
  |> List.map (List.fold_left (+) 0)

let part1 () =
  data
  |> List.fold_left max 0
  |> Printf.printf "%d\n%!"

let part2 () =
  data
  |> List.sort (Fun.flip compare)
  |> List.take 3
  |> List.fold_left (+) 0
  |> Printf.printf "%d\n%!"

let () =
  part2 ()
