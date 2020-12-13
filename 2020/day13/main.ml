open Containers

let start, busses =
  let [l1;l2] = CCIO.(with_in Sys.argv.(1) read_lines_l) in
  let start = int_of_string l1 in
  let busses = String.split_on_char ',' l2 |> List.filter_map int_of_string_opt in
  start, busses

let part1 () =
  List.map (fun x -> x, x - (start mod x)) busses
  |> List.sort (fun (_, x) (_, y) -> compare x y)
  |> List.hd
  |> (fun (x, d) -> d * x)

let busses =
  let [_;l2] = CCIO.(with_in Sys.argv.(1) read_lines_l) in
  String.split_on_char ',' l2
  |> List.mapi (fun i s -> Option.map (fun x -> (i, x)) (int_of_string_opt s) )
  |> List.filter_map Fun.id

(* from wikipedia *)
let eea a b =
  let s = ref 0 and old_s = ref 1 in
  let r = ref b and old_r = ref a in
  let t = ref 1 and old_t = ref 0 in
  let q = ref 0 in
  while !r <> 0 do
    q := !old_r / !r;
    let rr = !r in
    r := !old_r - !q * !r;
    old_r := rr;
    let ss = !s in
    s := !old_s - !q * !s;
    old_s := ss;
    let tt = !t in
    t := !old_t - !q * !t;
    old_t := tt;
  done;
  (!old_s, !old_t, !old_r)

let () =
  let nn = List.fold_left (fun acc (_, ni) -> acc * ni) 1 busses in
  Printf.printf "N: %d\n" nn;
  List.iter (fun (ai, ni) -> Printf.printf "(%d, %d) " ai ni) busses; print_endline "";
  let busses = List.map (fun (ai, ni) -> (ni - ai, ni)) busses in
  List.iter (fun (ai, ni) -> Printf.printf "(%d, %d) " ai ni) busses; print_endline "";
  let x =
    List.map (fun (ai, ni) ->
        let nni = nn / ni in
        let (mmi, mi, d) = eea nni ni in
        assert (mmi * nni + mi * ni = 1);
        assert (d = 1);
        ai, mmi, nni
      ) busses
    |> List.fold_left (fun acc (ai, mmi, nni) -> acc + ai * mmi * nni) 0
  in
  let x' =
    if x < 0 then
      OSeq.iterate x ((+) nn) |> OSeq.find (fun x -> x >= 0) |> Option.get_exn
    else
      OSeq.iterate x (fun x -> x - nn) |> OSeq.find (fun x -> x < nn) |> Option.get_exn
  in
  Printf.printf "%d\n" x'
