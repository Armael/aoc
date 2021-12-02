open Containers

let data = 
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s -> Scanf.sscanf s "%s %d" (fun x y -> (x, y)))
  |> List.map (fun (dir, x) ->
    (match dir with
     | "forward" -> `Forward
     | "down" -> `Down
     | "up" -> `Up
     | _ -> failwith "ohno"),
    x)

let part1 () =
  let x, y =
    List.fold_left (fun (x, y) (dir, incr) ->
      match dir with
      | `Forward -> (x + incr, y)
      | `Down -> (x, y + incr)
      | `Up -> (x, y - incr)
    ) (0, 0) data in
  Printf.printf "%d\n" (x * y)

let part2 () =
  let x, y, _aim =
    List.fold_left (fun (x, y, aim) (dir, incr) ->
      match dir with
      | `Forward -> (x + incr, y + aim * incr, aim)
      | `Down -> (x, y, aim + incr)
      | `Up -> (x, y, aim - incr)
    ) (0, 0, 0) data in
  Printf.printf "%d\n" (x * y)

let () = part2 ()
