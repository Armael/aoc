open Containers
let (===) = Char.equal

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s -> String.to_array s)
  |> Array.of_list

let next (cfg: char array array) =
  let cfg' = Array.map Array.copy cfg in

  let is_occupied i j =
    try cfg.(i).(j) === '#' with
      Invalid_argument _ -> false
  in

  let occupied_around i j =
    [is_occupied (i-1) j;
     is_occupied (i+1) j;
     is_occupied i (j-1);
     is_occupied i (j+1);
     is_occupied (i-1) (j-1);
     is_occupied (i-1) (j+1);
     is_occupied (i+1) (j-1);
     is_occupied (i+1) (j+1);
    ] |> List.map (function true -> 1 | false -> 0) |> List.fold_left (+) 0
  in

  for i = 0 to Array.length cfg - 1 do
    for j = 0 to Array.length cfg.(0) - 1 do
      if cfg.(i).(j) === 'L' && occupied_around i j = 0 then
        cfg'.(i).(j) <- '#'
      else if cfg.(i).(j) === '#' && occupied_around i j >= 4 then
        cfg'.(i).(j) <- 'L'
    done
  done;

  cfg'

let count_occupied cfg =
  Array.fold_left (fun sum a ->
    Array.fold_left (fun sum x ->
      if x === '#' then sum + 1 else sum
    ) sum a
  ) 0 cfg

let part1 () =
  let rec loop cfg =
    let cfg' = next cfg in
    if Stdlib.(cfg' = cfg) then count_occupied cfg
    else loop cfg'
  in
  loop data


let next2 (cfg: char array array) =
  let cfg' = Array.map Array.copy cfg in

  let rec is_occupied i j di dj =
    let i' = i + di and j' = j + dj in
    match cfg.(i').(j') with
    | exception Invalid_argument _ -> false
    | 'L' -> false
    | '#' -> true
    | _ -> is_occupied i' j' di dj
  in

  let occupied_around i j =
    [is_occupied i j (-1) 0;
     is_occupied i j 1 0;
     is_occupied i j 0 (-1);
     is_occupied i j 0 1;
     is_occupied i j (-1) (-1);
     is_occupied i j (-1) 1;
     is_occupied i j 1 (-1);
     is_occupied i j 1 1;
    ] |> List.map (function true -> 1 | false -> 0) |> List.fold_left (+) 0
  in

  for i = 0 to Array.length cfg - 1 do
    for j = 0 to Array.length cfg.(0) - 1 do
      if cfg.(i).(j) === 'L' && occupied_around i j = 0 then
        cfg'.(i).(j) <- '#'
      else if cfg.(i).(j) === '#' && occupied_around i j >= 5 then
        cfg'.(i).(j) <- 'L'
    done
  done;

  cfg'

let part2 () =
  let rec loop cfg =
    let cfg' = next2 cfg in
    if Stdlib.(cfg' = cfg) then count_occupied cfg
    else loop cfg'
  in
  loop data

let () =
  Printf.printf "%d\n%!" (part2 ())
