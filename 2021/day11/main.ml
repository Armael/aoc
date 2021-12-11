open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s ->
    String.to_array s |> Array.map (fun c -> Char.code c - Char.code '0')
  ) |> Array.of_list

let around (i, j) =
  List.product CCPair.make [-1; 0; 1] [-1; 0; 1]
  |> List.filter (fun (i, j) -> not (i = 0 && j = 0))
  |> List.map (fun (di, dj) -> (i + di, j + dj))
  |> List.filter (fun (i, j) -> 0 <= i && i < 10 && 0 <= j && j < 10)

let step st =
  Array.iter (fun a -> Array.iteri (fun i x -> a.(i) <- x+1) a) st;
  let todo = ref [] in
  Array.iteri (fun i a ->
    Array.iteri (fun j x ->
      if x > 9 then todo := (i, j) :: !todo
    ) a
  ) st;
  while not (List.is_empty !todo) do
    let (i, j) = List.hd !todo in
    todo := List.tl !todo;
    List.iter (fun (i', j') ->
      st.(i').(j') <- st.(i').(j') + 1;
      if st.(i').(j') = 10 then
        todo := (i', j') :: !todo
    ) (around (i, j))
  done;
  let flashes = ref 0 in
  Array.iteri (fun i a ->
    Array.iteri (fun j x ->
      if x > 9 then (
        st.(i).(j) <- 0;
        incr flashes
      )
    ) a
  ) st;
  !flashes

let print_grid st =
  Array.iter (fun a ->
    Array.iter print_int a;
    print_newline ()
  ) st;
  print_newline ()

let part1 n =
  let flashes = ref 0 in
  (* print_grid data; *)
  for _i = 1 to n do
    flashes := !flashes + step data;
    (* print_grid data *)
  done;
  Printf.printf "%d\n" !flashes

(* let () = part1 100 *)

let part2 () =
  OSeq.iterate 1 succ
  |> OSeq.map (fun ngen -> ngen, step data)
  |> OSeq.find (fun (_ngen, flashes) -> flashes = 100)
  |> Option.get_exn_or "ohno" |> fst |> Printf.printf "%d\n"

let () = part2 ()
