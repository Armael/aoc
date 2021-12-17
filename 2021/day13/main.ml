open Containers

let dots, folds =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.filter_map (fun s ->
       match String.split_on_char ',' s with
       | [a; b] -> Some (`Dot (int_of_string a, int_of_string b))
       | [""] -> None
       | [s] -> Scanf.sscanf s "fold along %c=%d"
                  (fun axis n -> Some (`Fold (axis, n)))
       | _ -> failwith "ono"
  )
  |> List.partition_filter_map (function
    | `Dot (x, y) -> `Left (x, y)
    | `Fold (a, n) -> `Right (a, n))

module DSet = Set.Make (struct
    type t = int * int
    let compare = Stdlib.compare
end)

let dots = DSet.of_list dots

let fold_up (yy: int) (s: DSet.t): DSet.t =
  DSet.map (fun (x, y) ->
    if y < yy then (x, y)
    else (x, yy - (y - yy))) s

let fold_left (xx: int) (s: DSet.t): DSet.t =
  DSet.map (fun (x, y) ->
    if x < xx then (x, y)
    else (xx - (x - xx), y)) s

let fold (a, n) =
  match a with
  | 'x' -> fold_left n
  | 'y' -> fold_up n
  | _ -> assert false

let part1 () =
  fold (List.hd folds) dots
  |> DSet.cardinal
  |> Printf.printf "%d\n"

let print_dots s =
  let (mx, my) = DSet.fold (fun (x, y) (mx, my) -> (max x mx, max y my)) s (0, 0) in
  for y = 0 to my do
    for x = 0 to mx do
      if DSet.mem (x, y) s then Printf.printf "#"
      else Printf.printf " "
    done;
    Printf.printf "\n"
  done

let () =
  List.fold_left (Fun.flip fold) dots folds
  |> print_dots
