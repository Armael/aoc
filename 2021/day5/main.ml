open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s ->
    Scanf.sscanf s "%d,%d -> %d,%d" (fun a b c d -> ((a, b), (c, d))))
  |> List.filter_map (fun ((x1, y1), (x2, y2)) ->
    if x1 = x2 then Some (`V (x1, min y1 y2, max y1 y2))
    else if y1 = y2 then Some (`H (min x1 x2, max x1 x2, y1))
    else if abs(x2-x1) = abs(y2-y1) then Some (`D (x1, y1, x2-x1, y2-y1))
    else None
  )

module H = Hashtbl.Make (struct
    type t = int * int
    let hash = Hashtbl.hash
    let equal = Pair.equal Int.equal Int.equal
  end)

let draw tbl w h =
  for y = 0 to h-1 do
    for x = 0 to w-1 do
      match H.find tbl (x, y) with
      | n -> print_int n
      | exception Not_found -> print_char '.'
    done;
    print_newline ()
  done

let h = H.create 8000

let add_pos x y =
  match H.find_opt h (x, y) with
  | Some n -> H.replace h (x, y) (n+1)
  | None -> H.replace h (x, y) 1

let part1 () =
  List.iter (function
    | `V (x, y1, y2) ->
      for y = y1 to y2 do add_pos x y done
    | `H (x1, x2, y) ->
      for x = x1 to x2 do add_pos x y done
    | `D _ -> ()
  ) data;
  H.fold (fun _ n count -> if n > 1 then count+1 else count) h 0
  |> Printf.printf "%d\n"

(* let () = part1 () *)

let part2 () =
  List.iter (function
    | `V (x, y1, y2) ->
      for y = y1 to y2 do add_pos x y done
    | `H (x1, x2, y) ->
      for x = x1 to x2 do add_pos x y done
    | `D (x, y, dx, dy) ->
      List.combine List.(0 -- dx) List.(0 -- dy)
      |> List.iter (fun (dx, dy) -> add_pos (x+dx) (y+dy))
  ) data;
  H.fold (fun _ n count -> if n > 1 then count+1 else count) h 0
  |> Printf.printf "%d\n"

let () = part2 ()
