open Containers

let data =
  let data_l = CCIO.(with_in Sys.argv.(1) read_lines_l) in
  let rec loop l =
    match l with
    | [] -> []
    | "" :: ss -> loop ss
    | _ :: ss ->
      let dots, ss = List.take_drop_while (Fun.negate String.is_empty) ss in
      List.map (fun s -> Scanf.sscanf s "%d,%d,%d" (fun x y z -> (x, y, z))) dots
      :: loop ss
  in
  loop data_l |> Array.of_list

let rotations =
  OSeq.product3 (OSeq.of_list [0;1;2;3]) (OSeq.of_list [0;1;2;3]) (OSeq.of_list [0;1;2;3])
  |> OSeq.map (fun (a, b, c) ->
    let cos = function 0 -> 1 | 1 -> 0 | 2 -> -1 | _ -> 0 in
    let sin = function 0 -> 0 | 1 -> 1 | 2 -> 0 | _ -> -1 in
    (cos a * cos b,
     cos a * sin b * sin c - sin a * cos c,
     cos a * sin b * cos c + sin a * sin c,
     sin a * cos b,
     sin a * sin b * sin c + cos a * cos c,
     sin a * sin b * cos c - cos a * sin c,
     - sin b,
     cos b * sin c,
     cos b * cos c))
  |> OSeq.sort_uniq ~cmp:Stdlib.compare

let () = OSeq.length rotations |> Printf.printf "rotations: %d\n"

let rot3 rot (x, y, z) =
  let (a, b, c, d, e, f, g, h, i) = rot in
  (x * a + y * b + z * c,
   d * x + e * y + f * z,
   g * x + h * y + i * z)

let rotate dots rot =
  List.map (rot3 rot) dots

let transl dots v =
  let (dx, dy, dz) = v in
  List.map (fun (x, y, z) -> x + dx, y + dy, z + dz) dots

let scan_rotations = Array.init (Array.length data) (fun i ->
  List.map (rotate data.(i)) (OSeq.to_list rotations))

module PSet = Set.Make (struct
    type t = int * int * int
    let compare = Stdlib.compare
end)

let trymatch ?(dbg=false) scan0 scan1 =
  let set0 = PSet.of_list data.(scan0) in
  List.find_map (fun dots1 ->
    if dbg then (
      Printf.printf "dots1 (rotated, before transl):\n";
      List.iter (fun (x,y,z) -> Printf.printf "%d,%d,%d\n" x y z) dots1;
      Printf.printf "\n";
    );
    OSeq.find_map (fun (x0, y0, z0) ->
      if dbg then Printf.printf "v0: %d,%d,%d\n" x0 y0 z0;
      List.find_map (fun (x1, y1, z1) ->
        if dbg then Printf.printf "v1: %d,%d,%d\n\n" x1 y1 z1;
        let dv1 = (x0 - x1, y0 - y1, z0 - z1) in
        let dots1 = transl dots1 dv1 in

        if dbg then (
          Printf.printf "dots1 (translated):\n";
          List.iter (fun (x,y,z) -> Printf.printf "%d,%d,%d\n" x y z) dots1;
          Printf.printf "\n";
        );

        let set1 = PSet.of_list dots1 in
        let seti = PSet.inter set0 set1 in
        if PSet.cardinal seti >= 12 then
          Some (dv1, seti, dots1)
        else None
      ) dots1
    ) (OSeq.of_list data.(scan0))
  ) scan_rotations.(scan1)

module ISet = Set.Make (Int)

let solve () =
  let remaining = ISet.of_list List.(1 --^ Array.length data) in
  let dvs = Array.make (Array.length data) (0, 0, 0) in
  let rec loop good remaining =
    if ISet.is_empty remaining then () else begin
      let scan0, good = List.hd_tl good in
      let res = ISet.to_seq remaining |> OSeq.filter_map (fun scan1 ->
        trymatch scan0 scan1
        |> Option.map (fun res ->
          Printf.printf "found match %d->%d\n%!" scan0 scan1;
          scan1, res
        )) in
      let new_good = List.map (fun (scan1, (dv, _, dots1)) ->
        data.(scan1) <- dots1;
        dvs.(scan1) <- dv;
        scan1)
        (OSeq.to_list res) in

      loop (new_good @ good @ [scan0])
        (List.fold_left (fun remaining x -> ISet.remove x remaining) remaining new_good)
    end
  in
  loop [0] remaining;
  Array.fold (fun beacons dots ->
    List.fold_left (fun beacons dot -> PSet.add dot beacons) beacons dots
  ) PSet.empty data,
  dvs

let dist (x0, y0, z0) (x1, y1, z1) =
  abs (x1 - x0) + abs (y1 - y0) + abs (z1 - z0)

let () =
  let beacons, dvs = solve () in
  PSet.cardinal beacons |> Printf.printf "%d\n";
  OSeq.product (OSeq.of_array dvs) (OSeq.of_array dvs)
  |> OSeq.map (fun (v1, v2) -> dist v1 v2)
  |> OSeq.max ~lt:(<)
  |> Printf.printf "%d\n"
