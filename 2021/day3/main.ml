open Containers

let data = 
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s ->
    String.to_array s |> Array.map (fun c -> Char.code c - Char.code '0'))

let bits data i =
  List.map (fun a -> a.(i)) data

let choose_bit bits =
  let n = List.fold_left (+) 0 bits in
  if n > List.length bits / 2 then 1 else 0

let bits2dec bits =
  snd @@ List.fold_right (fun b (pow, acc) ->
    (2 * pow, if b > 0 then acc + pow else acc)
  ) bits (1,0)

let part1 () =
  let gamma =
    List.(0 --^ Array.length (List.hd data))
    |> List.map (bits data)
    |> List.map choose_bit in
  let eps = List.map (function 0 -> 1 | 1 -> 0 | _ -> assert false) gamma in
  Printf.printf "%d\n" (bits2dec gamma * bits2dec eps)

(* let () = part1 () *)

let choose_bit bits =
  let n = List.fold_left (+) 0 bits in
  let len = List.length bits in
  if 2 * n > len then `One else
  if 2 * n = len then `Eq else
    `Zero

let part2 () =
  let rec loop ~pref data i =
    match data with
    | [x] -> Array.to_list x |> bits2dec
    | _ ->
      let bit =
        match choose_bit @@ bits data i with
        | `One -> pref | `Zero -> (1-pref)
        | `Eq -> pref in
      loop ~pref (List.filter (fun a -> a.(i) = bit) data) (i+1)
  in
  let ox = loop ~pref:1 data 0 in
  let co2 = loop ~pref:0 data 0 in
  Printf.printf "%d\n" (ox * co2)

let () = part2 ()
