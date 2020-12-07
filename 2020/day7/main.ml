open Containers

let parse_line s =
  let [k;vss] = String.split ~by:" bags contain " s in
  if String.equal vss "no other bags." then k, []
  else
    let vs = String.split ~by:", " vss in
    let vs = List.map (fun v -> Scanf.sscanf v "%d %s %s" (fun x a b -> x, a ^ " " ^ b)) vs in
    k, vs

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map parse_line

let h = Hashtbl.create 37

let () =
  List.iter (fun (k, vs) ->
    List.iter (fun (_, v) ->
      Hashtbl.add h v k
    ) vs
  ) data

module HS = CCHashSet.Make(String)

let seen = HS.create 37
let count = ref 0

let rec trav x =
  if HS.mem seen x then () else begin
    incr count;
    HS.insert seen x;
    let containers = Hashtbl.find_all h x in
    List.iter trav containers
  end

let () =
  trav "shiny gold";
  Printf.printf "%d\n%!" (!count - 1)


let h2 = Hashtbl.create 37

let () =
  List.iter (fun (k, vs) ->
    List.iter (fun (n, v) ->
      Hashtbl.add h2 k (n, v)
    ) vs
  ) data

let count = Hashtbl.create 37

let rec trav x =
  match Hashtbl.find_opt count x with
  | Some y -> y
  | None ->
     let contains = Hashtbl.find_all h2 x in
     let cnt =
       List.fold_left (fun acc (n, x') ->
         acc + n * (trav x')
       ) 1 contains in
     Hashtbl.replace count x cnt;
     cnt

let () =
  Printf.printf "%d\n%!" (trav "shiny gold" - 1)
