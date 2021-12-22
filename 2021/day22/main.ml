open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s -> Scanf.sscanf s "%s x=%d..%d,y=%d..%d,z=%d..%d"
                  (fun toggle x0 x1 y0 y1 z0 z1 ->
                     (match toggle with "on" -> true | "off" -> false | _ -> assert false),
                     (x0, y0, z0),
                     (x1, y1, z1)))

let grid =
  Array.init 101 (fun _ ->
    Array.init 101 (fun _ ->
      Bytes.make 101 '0'))

let iter_subgrid (x0, y0, z0) (x1, y1, z1) f =
  for x = max x0 (-50) to min x1 50 do
    for y = max y0 (-50) to min y1 50 do
      for z = max z0 (-50) to min z1 50 do
        f x y z
      done
    done
  done

let set_grid x y z b =
  Bytes.set grid.(x+50).(y+50) (z+50)
    (if b then '1' else '0')

let get_grid x y z =
  match Bytes.get grid.(x+50).(y+50) (z+50) with
  | '1' -> true
  | _ -> false

let part1 () =
  List.iter (fun (toggle, v0, v1) ->
    iter_subgrid v0 v1 (fun x y z ->
      set_grid x y z toggle
    )
  ) data;
  let count = ref 0 in
  iter_subgrid (-50, -50, -50) (50, 50, 50) (fun x y z ->
    if get_grid x y z then incr count
  );
  Printf.printf "%d\n" !count

(* *** *)

let (minx, miny, minz), (maxx, maxy, maxz) = List.fold_left
    (fun ((minx, miny, minz), (maxx, maxy, maxz))
      (_, (x0, y0, z0), (x1, y1, z1)) ->
      (min minx x0, min miny y0, min minz z0),
      (max maxx x1, max maxy y1, max maxz z1))
    ((0, 0, 0), (0, 0, 0))
    data

open Gg

let to_box ((x0, y0, z0), (x1, y1, z1)) =
  Box3.v
    (V3.v (float x0) (float y0) (float z0))
    (V3.v (float (x1 - x0 + 1)) (float (y1 - y0 + 1)) (float (z1 - z0 + 1)))

let bbox = to_box ((minx, miny, minz), (maxx, maxy, maxz))

let carve_out core box =
  let cuts = [
    (* front *)
    Box3.v (Box3.o bbox)
      (V3.v (Box3.w bbox) (Box3.h bbox) (Box3.oz core -. Box3.oz bbox));
    (* bottom *)
    Box3.v
      (V3.v (Box3.ox bbox) (Box3.oy bbox) (Box3.oz core))
      (V3.v (Box3.w bbox) (Box3.oy core -. Box3.oy bbox) (Box3.d bbox));
    (* left *)
    Box3.v
      (V3.v (Box3.ox bbox) (Box3.oy core) (Box3.oz core))
      (V3.v (Box3.ox core -. Box3.ox bbox) (Box3.h bbox) (Box3.d bbox));
    (* top *)
    Box3.v
      (V3.v (Box3.ox core) (Box3.oy core +. Box3.h core) (Box3.oz core))
      (V3.v (Box3.w bbox) (Box3.h bbox) (Box3.d bbox));
    (* right *)
    Box3.v
      (V3.v (Box3.ox core +. Box3.w core) (Box3.oy core) (Box3.oz core))
      (V3.v (Box3.w bbox) (Box3.h core) (Box3.d bbox));
    (* back *)
    Box3.v
      (V3.v (Box3.ox core) (Box3.oy core) (Box3.oz core +. Box3.d core))
      (V3.v (Box3.w core) (Box3.h core) (Box3.d bbox));
  ] in
  List.map (fun cut -> Box3.inter box cut) cuts
  |> List.filter (Fun.negate Box3.is_empty)
  |> List.sort_uniq ~cmp:Box3.compare

let data = List.map (fun (toggle, v0, v1) -> (toggle, to_box (v0, v1))) data

let rec loop actions boxes =
  Printf.printf "boxes: %d\n%!" (List.length boxes);
  let boxes = List.sort_uniq ~cmp:Box3.compare boxes in
  match actions with
  | [] -> boxes
  | (toggle, abox) :: actions ->
    let boxes =
      (if toggle then [abox] else []) @
      List.flat_map (fun box ->
        let i = Box3.inter abox box in
        if Box3.is_empty i then [box] else carve_out i box
      ) boxes
    in
    loop actions boxes

let () =
  let boxes = loop data [] in
  List.fold_left (fun count box ->
    count + int_of_float ((Box3.w box) *. (Box3.h box) *. (Box3.d box)))
    0 boxes
  |> Printf.printf "%d\n"
