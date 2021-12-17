open Containers

let x0, x1, y0, y1 =
  Scanf.sscanf Sys.argv.(1) "target area: x=%d..%d, y=%d..%d" (fun x0 x1 y0 y1 ->
    (x0, x1, y0, y1))

(* let diag x y =
 *   sqrt ((float x) ** 2. +. (float y) ** 2.) +. 1.
 *   |> int_of_float
 *
 * let area_diag = diag (x1 - x0) (y1 - y0) *)

let step (x, y) (dx, dy) =
  (x + dx, y + dy),
  ((if dx > 0 then dx-1 else if dx < 0 then dx+1 else dx),
   dy - 1)

let is_final (x, y) =
  x >= x0 && x <= x1 && y >= y0 && y <= y1

let is_miss (x, y) =
  y < y0 || x > x1

let seq = OSeq.unfold (fun (x, y) ->
  let next =
    if x = 0 then (y+1, 0)
    else (x-1, y+1)
  in
  Some ((x, y), next)
) (0, 0)

let rec run maxh pos dpos =
  if is_final pos then `Ok maxh
  else if is_miss pos then `Miss dpos
  else
    let pos', dpos' = step pos dpos in
    run (max maxh (snd pos')) pos' dpos'

let part1 () =
  OSeq.map (run 0 (0, 0)) seq
  |> OSeq.take 100_000
  |> OSeq.filter_map (function `Ok maxh -> Some maxh | _ -> None)
  |> OSeq.max ~lt:(<)
  |> Printf.printf "%d\n"

let seq = OSeq.unfold (fun (x, y) ->
  let next =
    if x = 0 && y >= 0 then (0, -y-1)
    else if y < 0 then (x+1, y+1)
    else (x-1, y+1)
  in
  Some ((x, y), next)
) (0, 0)

let part2 () =
  OSeq.map (fun v0 -> (run 0 (0, 0) v0), v0) seq
  |> OSeq.take 500_000
  |> OSeq.filter_map (function (`Ok maxh, _) -> Some maxh | _ -> None)
  |> OSeq.length
  |> Printf.printf "%d\n"

let () = part2 ()
