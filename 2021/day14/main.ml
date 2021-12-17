open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)

let template = List.hd data

let rules =
  (List.tl @@ List.tl data)
  |> List.map (fun s -> Scanf.sscanf s "%c%c -> %c" (fun c1 c2 y -> (c1, c2, y)))

let id c = Char.code c - Char.code 'A'
let ofid n = Char.chr (n + Char.code 'A')

let rules =
  let r = Array.make_matrix 26 26 (-1) in
  List.iter (fun (c1, c2, c) -> r.(id c1).(id c2) <- id c) rules;
  r

let step s =
  let b = Buffer.create (String.length s) in
  for i = 0 to String.length s - 2 do
    let n = rules.(id s.[i]).(id s.[i+1]) in
    if i = 0 then Buffer.add_char b s.[i];
    if n >= 0 then Buffer.add_char b (ofid n);
    Buffer.add_char b s.[i+1]
  done;
  Buffer.contents b

let part1 () =
  let s = ref template in
  for _ = 1 to 9 do
    (*Printf.printf "%s\n" !s;*)
    s := step !s
  done;
  let count = Array.make 26 0 in
  String.iter (fun c -> count.(id c) <- count.(id c) + 1) !s;
  (Array.to_seq count |> OSeq.max ~lt:(<)) -
  (Array.to_seq count |> OSeq.filter (fun x -> x > 0) |> OSeq.max ~lt:(>))
  |> Printf.printf "%d\n"


let mkpairs () = Array.make_matrix 26 26 0
let mkcount () = Array.make 26 0
let pairs_of_template s =
  let pairs = mkpairs () in
  for i = 0 to String.length s - 2 do
    pairs.(id s.[i]).(id s.[i+1]) <- pairs.(id s.[i]).(id s.[i+1]) + 1
  done;
  pairs
let count_of_template s =
  let count = mkcount () in
  String.iter (fun c -> count.(id c) <- count.(id c) + 1) s;
  count

let step pairs count =
  let pairs' = mkpairs () in
  for x = 0 to 25 do
    for y = 0 to 25 do
      let c = rules.(x).(y) in
      let npairs = pairs.(x).(y) in
      if npairs > 0 && c >= 0 then (
        pairs'.(x).(c) <- pairs'.(x).(c) + npairs;
        pairs'.(c).(y) <- pairs'.(c).(y) + npairs;
        count.(c) <- count.(c) + npairs
      )
    done
  done;
  pairs'

let () =
  let pairs = ref (pairs_of_template template) in
  let count = count_of_template template in
  for _ = 0 to 39 do pairs := step !pairs count done;
  (Array.to_seq count |> OSeq.max ~lt:(<)) -
  (Array.to_seq count |> OSeq.filter (fun x -> x > 0) |> OSeq.max ~lt:(>))
  |> Printf.printf "%d\n"
