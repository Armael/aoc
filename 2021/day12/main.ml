open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s ->
       match String.split_on_char '-' s with
       | [a; b] -> (a, b)
       | _ -> failwith "ono"
  )

module H = Hashtbl.Make(String)

let succs =
  let h = H.create 37 in
  List.iter (fun (a, b) ->
    H.add h a b;
    H.add h b a
  ) data;
  h

let is_big s =
  Char.code s.[0] >= Char.code 'A' &&
  Char.code s.[0] <= Char.code 'Z'

let part1 () =
  let step paths =
    List.flat_map (fun p ->
      let last = List.hd p in
      let ns =
        H.find_all succs last
        |> List.filter (fun n -> is_big n || not (List.mem n p))
      in
      List.map (fun n -> n::p) ns
    ) paths
  in
  let rec loop n paths =
    let paths' = step paths in
    let n', paths' = List.fold_left (fun (n', paths') p ->
      if String.equal (List.hd p) "end" then (n'+1, paths')
      else (n', p :: paths')
    ) (n, []) paths' in
    if not (List.is_empty paths') then loop n' paths'
    else n'
  in
  loop 0 [["start"]]
  |> Printf.printf "%d\n"

let () = part1 ()

let can_visit n (p, has_dup) =
  if String.equal n "start" then None else (
    if is_big n then Some (p, has_dup) else
      let cnt = List.count (String.equal n) p in
      if cnt = 0 then Some (p, has_dup)
      else if not has_dup && cnt = 1 then Some (p, true)
      else None
  )

let part2 () =
  let step paths =
    List.flat_map (fun (p: _ list * bool) ->
      let last = List.hd (fst p) in
      H.find_all succs last
      |> List.filter_map (fun n ->
        Option.map (fun (p', has_dup) -> (n :: p', has_dup)) (can_visit n p)
      )
    ) paths
  in
  let rec loop n paths =
    let paths' = step paths in
    let n', paths' = List.fold_left (fun (n', paths') p ->
      if String.equal (List.hd (fst p)) "end" then (n'+1, paths')
      else (n', p :: paths')
    ) (n, []) paths' in
    if not (List.is_empty paths') then loop n' paths'
    else n'
  in
  loop 0 [["start"], false]
  |> Printf.printf "%d\n"

let () = part2 ()
