open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s ->
    String.to_list s
    |> List.map (fun c -> Char.code c - Char.code '0')
    |> Array.of_list
  ) |> Array.of_list

let width = Array.length data.(0)
let height = Array.length data

module PSet = Set.Make (struct
    type t = int * int
    let compare = Stdlib.compare
end)

module Frontier = Psq.Make (struct
    type t = int * int
    let compare = Stdlib.compare
end)(Int)

let rec loop risk inbounds is_dest visited frontier =
  let ((nx, ny), d), frontier = Frontier.pop frontier |> Option.get_exn_or "a" in
  (* Printf.printf "%d, %d  [%d]\n" nx ny d; *)
  if is_dest nx ny then d else (
    let succs = [nx-1, ny; nx+1, ny;
                 nx, ny-1; nx, ny+1]
                |> List.filter inbounds
                |> List.filter (fun (x, y) -> not (PSet.mem (x, y) visited))
    in
    let visited = PSet.add (nx, ny) visited in
    let frontier = List.fold_left (fun frontier (nx', ny') ->
      Frontier.update (nx', ny') (function
        | None -> Some (d + risk (nx', ny'))
        | Some d' -> Some (min d' (d + risk (nx', ny'))))
        frontier)
      frontier succs in
    (* Printf.printf "frontier =";
     * Frontier.iter (fun (x, y) d -> Printf.printf " (%d, %d, %d)" x y d) frontier;
     * Printf.printf "\n"; *)
    loop risk inbounds is_dest visited frontier
  )

let is_dest nx ny = nx = width-1 && ny = height-1
let inbounds (x, y) = x >= 0 && y >= 0 && x < width && y < height
let risk (x, y) = data.(x).(y)

let () =
  loop risk inbounds is_dest PSet.empty (Frontier.sg (0, 0) 0)
  |> Printf.printf "%d\n"

let inbounds (x, y) =
  x >= 0 && y >= 0 &&
  x < 5 * width && y < 5 * height

let risk (x, y) =
  let tilex = x / width in
  let offx = x mod width in
  let tiley = y / height in
  let offy = y mod height in
  let r = data.(offx).(offy) in
  ((r - 1) + tilex + tiley) mod 9 + 1

let is_dest nx ny =
  nx = 5 * width - 1 && ny = 5 * height - 1

let () =
  loop risk inbounds is_dest PSet.empty (Frontier.sg (0, 0) 0)
  |> Printf.printf "%d\n"
