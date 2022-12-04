open Containers
let (=) = Stdlib.(=)

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s -> Scanf.sscanf s "%d-%d,%d-%d" (fun a b a' b' -> ((a, b), (a', b'))))

let range_included (a, b) (a', b') =
  a' <= a && b <= b'

let part1 () =
  data
  |> List.filter (fun (r1, r2) -> range_included r1 r2 || range_included r2 r1)
  |> List.length
  |> Printf.printf "%d\n"

let range_disjoint (a, b) (a', b') =
  b < a' || b' < a

let part2 () =
  data
  |> List.filter (fun (r1, r2) -> not (range_disjoint r1 r2))
  |> List.length
  |> Printf.printf "%d\n"

let () =
  part2 ()
