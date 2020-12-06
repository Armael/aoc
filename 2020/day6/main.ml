open Containers

let rec group acc = function
  | "" :: ll -> acc :: group [] ll
  | l :: ll -> group (l :: acc) ll
  | [] -> [acc]

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> group []

module CSet = Set.Make(Char)
  
let part1 () =
  data
  |> List.map (fun group ->
       List.fold_left (fun qs person ->
         String.fold (Fun.flip CSet.add) qs person
       ) CSet.empty group
  )
  |> List.map CSet.cardinal |> List.fold_left (+) 0

module CMap = Map.Make(Char)

let part2 () =
  data
  |> List.map (fun group ->
       List.length group,
       List.fold_left (fun qs person ->
         String.fold (fun qs c ->
           let i = Option.get_or ~default:0 (CMap.find_opt c qs) in
           CMap.add c (i+1) qs
         ) qs person
       ) CMap.empty group
  )
  |> List.map (fun (n, m) -> CMap.filter (fun _ i -> i = n) m |> CMap.cardinal)
  |> List.fold_left (+) 0

let () = Printf.printf "%d\n%!" (part2 ())
