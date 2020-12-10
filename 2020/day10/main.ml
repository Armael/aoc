open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.filter_map int_of_string_opt
  |> List.sort compare

let device = (List.fold_left max 0 data) + 3
let ports = 0 :: data @ [device]

let part1 () =
  let data = OSeq.of_list ports in
  OSeq.zip data (OSeq.drop 1 data)
  |> OSeq.fold_left (fun (n, m) (i, j) ->
       if j-i = 1 then (n+1, m)
       else if j-i = 3 then (n, m+1)
       else (n, m)
     ) (0, 0)
  |> (fun (n, m) ->
    Printf.printf "δ=1: %d, δ=3: %d\n" n m;
    n * m)

let h = Hashtbl.create 37

let () =
  let rec loop = function
    | x :: xs ->
       let ys = List.take_while (fun y -> y <= x + 3) xs in
       List.iter (fun y -> Hashtbl.add h x y) ys;
       loop xs
    | [] -> ()
  in
  loop ports

let part2 () =
  let cache = Hashtbl.create 37 in
  Hashtbl.add cache device 1;
  let rec count x =
    match Hashtbl.find_opt cache x with
    | Some n -> n
    | None ->
       let count_x =
         List.fold_left (fun n y -> n + count y) 0
            (Hashtbl.find_all h x)
       in
       Hashtbl.add cache x count_x;
       count_x
  in
  count 0

let () =
  Printf.printf "%d\n%!" (part2 ())
