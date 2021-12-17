open Containers

let data = Sys.argv.(1)

let hex_to_bits s =
  String.to_list s
  |> List.flat_map (fun c ->
    let n =
      if Char.code c >= Char.code '0' && Char.code c <= Char.code '9' then
        Char.code c - Char.code '0'
      else
        10 + Char.code c - Char.code 'A'
    in
    [n land 0x8 > 0; n land 0x4 > 0; n land 0x2 > 0; n land 0x1 > 0]
  )

let bits_to_num (bs: bool List.t) =
  List.fold_left (fun (n, pow) b -> if b then (n + pow, pow*2) else (n, pow*2)) (0, 1)
    (List.rev bs)
  |> fst

let read_lit (bs: bool List.t): int * bool List.t =
  let rec loop bs =
    let b, bs = List.hd_tl bs in
    let b4, bs = List.take_drop 4 bs in
    if b then
      let rest, bs = loop bs in
      b4 @ rest, bs
    else
      b4, bs
  in
  let bits, bs = loop bs in
  bits_to_num bits, bs

let read3 bs =
  let b3, bs = List.take_drop 3 bs in
  bits_to_num b3, bs


let read_lentyid bs =
  let b, bs = List.hd_tl bs in
  if not b then
    let len, bs = List.take_drop 15 bs in
    `Len (bits_to_num len), bs
  else
    let nb, bs = List.take_drop 11 bs in
    `Nb (bits_to_num nb), bs

type pak_data =
  | Lit of int
  | Paks of int * pak list

and pak = int * pak_data

let rec read_pak bs =
  let version, bs = read3 bs in
  let tyid, bs = read3 bs in
  match tyid with
  | 4 ->
    let l, bs = read_lit bs in
    (version, Lit l), bs
  | _ ->
    let lentyid, bs = read_lentyid bs in
    match lentyid with
    | `Len n ->
      let subbs, bs = List.take_drop n bs in
      let paks, _ = read_paks None subbs in
      (version, Paks (tyid, paks)), bs
    | `Nb n ->
      let paks, bs = read_paks (Some n) bs in
      (version, Paks (tyid, paks)), bs

and read_paks num bs =
  if List.length bs < 6 || Stdlib.(num = Some 0) then [], bs
  else
    let pak, bs = read_pak bs in
    let num = Option.map pred num in
    let paks, bs = read_paks num bs in
    pak :: paks, bs

let rec pak_versions (v, pk) =
  v ::
  match pk with
  | Lit _ -> []
  | Paks (_, ps) -> paks_versions ps
and paks_versions ps = List.flat_map pak_versions ps

let () =
  let bs = hex_to_bits data in
  let pk, _ = read_pak bs in
  pak_versions pk
  |> List.fold_left (+) 0
  |> Printf.printf "%d\n"

let rec eval (p: pak) =
  match snd p with
  | Lit n -> n
  | Paks (id, pks) ->
    let ns = List.map eval pks in
    match id with
    | 0 -> List.fold_left (+) 0 ns
    | 1 -> List.fold_left ( * ) 1 ns
    | 2 -> List.fold_left min (List.hd ns) (List.tl ns)
    | 3 -> List.fold_left max (List.hd ns) (List.tl ns)
    | 5 ->
      let x = List.hd ns and y = List.hd (List.tl ns) in
      if x > y then 1 else 0
    | 6 ->
      let x = List.hd ns and y = List.hd (List.tl ns) in
      if x < y then 1 else 0
    | 7 ->
      let x = List.hd ns and y = List.hd (List.tl ns) in
      if x = y then 1 else 0
    | _ ->
      assert false

let () =
  let bs = hex_to_bits data in
  let pk, _ = read_pak bs in
  Printf.printf "%d\n" (eval pk)
