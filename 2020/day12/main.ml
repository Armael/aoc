open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s -> Scanf.sscanf s "%c%d" (fun c x -> (c, x)))

let rot (dx, dy) d =
  match d with
  | 90 | -270 -> (-dy, dx)
  | 180 | -180 -> (-dx, -dy)
  | 270 | -90 -> (dy, -dx)
  | 0 -> (dx, dy)
  | _ -> assert false

let rec loop (x, y) (dx, dy) =
  Printf.printf "(%d, %d) δx:%d δy:%d\n" x y dx dy;
  function
  | [] -> (x, y)
  | (c, arg) :: ops ->
     match c with
     | 'N' -> loop (x, y+arg) (dx, dy) ops
     | 'S' -> loop (x, y-arg) (dx, dy) ops
     | 'E' -> loop (x+arg, y) (dx, dy) ops
     | 'W' -> loop (x-arg, y) (dx, dy) ops
     | 'L' -> loop (x, y) (rot (dx, dy) arg) ops
     | 'R' -> loop (x, y) (rot (dx, dy) (-arg)) ops
     | 'F' -> loop (x+dx*arg, y+dy*arg) (dx, dy) ops
     | _ -> assert false

let part1 () =
  let (x', y') = loop (0, 0) (1, 0) data in
  abs x' + abs y'


let rec loop (x, y) (dx, dy) =
  Printf.printf "(%d, %d) δx:%d δy:%d\n" x y dx dy;
  function
  | [] -> (x, y)
  | (c, arg) :: ops ->
     match c with
     | 'N' -> loop (x, y) (dx, dy+arg) ops
     | 'S' -> loop (x, y) (dx, dy-arg) ops
     | 'E' -> loop (x, y) (dx+arg, dy) ops
     | 'W' -> loop (x, y) (dx-arg, dy) ops
     | 'L' -> loop (x, y) (rot (dx, dy) arg) ops
     | 'R' -> loop (x, y) (rot (dx, dy) (-arg)) ops
     | 'F' -> loop (x+dx*arg, y+dy*arg) (dx, dy) ops
     | _ -> assert false

let part2 () =
  let (x', y') = loop (0, 0) (10, 1) data in
  abs x' + abs y'

let () =
  Printf.printf "%d\n%!" (part2 ())
