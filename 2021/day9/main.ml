open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s ->
    String.to_array s |> Array.map (fun c -> Char.code c - Char.code '0'))
  |> Array.of_list

let data_w = Array.length data.(0)
let data_h = Array.length data

let around (i, j) =
  [(0,1); (0,-1); (1, 0); (-1, 0)]
  |> List.map (fun (dx,dy) -> (i + dx, j + dy))
  |> List.filter (fun (i, j) -> i >= 0 && i < data_h && j >= 0 && j < data_w)

let is_lowpoint (i, j) =
  List.for_all (fun (i', j') -> data.(i').(j') > data.(i).(j)) (around (i, j))

let part1 () =
  OSeq.(product (0 --^ data_h) (0 --^ data_w))
  |> OSeq.filter is_lowpoint
  |> OSeq.map (fun (i, j) -> data.(i).(j) + 1)
  |> OSeq.sum
  |> Printf.printf "%d\n"

let () = part1 ()

module ISet = Set.Make(struct
    type t = int * int
    let compare = Stdlib.compare
  end)

let basin (i, j) =
  let rec loop explored todo =
    match ISet.choose_opt todo with
    | None -> explored
    | Some (i, j) ->
      let todo = ISet.remove (i, j) todo in
      if data.(i).(j) = 9 then loop explored todo else
        let todo =
          ISet.add_list todo @@
          List.filter (fun pos -> not (ISet.mem pos explored)) (around (i, j))
        in
        loop (ISet.add (i, j) explored) todo
  in
  loop ISet.empty (ISet.singleton (i, j)) |> ISet.cardinal

let part2 () =
  OSeq.(product (0 --^ data_h) (0 --^ data_w))
  |> OSeq.filter is_lowpoint
  |> OSeq.map basin
  |> OSeq.sort ~cmp:(Fun.flip compare)
  |> OSeq.take 3
  |> OSeq.fold ( * ) 1
  |> Printf.printf "%d\n"

let () = part2 ()
