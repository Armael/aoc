open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_all)
  |> String.trim
  |> String.split_on_char ','
  |> List.map int_of_string

let step (fishes: int array): int array =
  let fishes' = Array.make 9 0 in
  fishes'.(6) <- fishes.(0);
  fishes'.(8) <- fishes.(0);
  for i = 1 to 8 do
    fishes'.(i-1) <- fishes'.(i-1) + fishes.(i)
  done;
  fishes'

let go nb_gen =
  let fishes0 = Array.make 9 0 in
  List.iter (fun fish -> fishes0.(fish) <- fishes0.(fish) + 1) data;
  let fishes = ref fishes0 in
  for _i = 1 to nb_gen do fishes := step !fishes done;
  let nb_fishes = Array.fold (+) 0 !fishes in
  Printf.printf "%d\n" nb_fishes

let () = go 80
let () = go 256
