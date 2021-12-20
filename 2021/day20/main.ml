open Containers

let algo, img =
  let data_l = CCIO.(with_in Sys.argv.(1) read_lines_l) in
  let dat1, dat2 = List.take_drop_while (Fun.negate String.is_empty) data_l in
  let dat2 = List.tl dat2 |> List.map Bytes.of_string in
  let algo = String.concat "" dat1 in
  assert (String.length algo = 512);
  algo, Array.of_list dat2

let int_of_bits bits =
  List.fold_left (fun (n, pow) b -> if b then (n+pow, pow*2) else (n, pow*2)) (0, 1)
    (List.rev bits) |> fst

let step (img, background) =
  let img' = Array.init (Array.length img + 2)
      (fun _ -> Bytes.make (Bytes.length img.(0) + 2) 'x')
  in
  let read (x, y) =
    match Bytes.get img.(y) (x) with
    | '.' -> false
    | '#' -> true
    | _ -> assert false
    | exception Invalid_argument _ -> background
  in
  for y = 0 to Array.length img + 1 do
    for x = 0 to Bytes.length img.(0) + 1 do
      [(-1, -1); (0, -1); (1, -1); (-1, 0); (0, 0); (1, 0); (-1, 1); (0, 1); (1, 1)]
      |> List.map (fun (dx, dy) -> (x+dx, y+dy))
      |> List.map (fun (x, y) -> read (x-1, y-1))
      |> int_of_bits
      |> fun i -> Bytes.set img'.(y) x algo.[i]
    done
  done;
  let background' =
    List.init 9 (fun _ -> background)
    |> int_of_bits
    |> String.get algo
  in
  img', Char.(background' = '#')

let pp img =
  Array.iter (fun b ->
    Bytes.iter print_char b;
    print_newline ()
  ) img;
  print_newline ()

let rec loop n img =
  if n = 0 then img
  else loop (n-1) (step img)

let () =
  let (img', background') = loop 50 (img, false) in
  assert (not background');
  Printf.printf "%d\n" @@
  Array.fold (fun acc b ->
    Bytes.fold_left (fun acc c ->
      if Char.(c = '#') then acc + 1 else acc) acc b) 0 img'
