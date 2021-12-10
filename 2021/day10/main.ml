open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map String.to_list

exception Illegal of char

let score = function
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | _ -> assert false

let eval line =
  try
    let stk =
      List.fold_left (fun stk c ->
        match c, stk with
        | ('(' | '[' | '{' | '<'), _ -> c :: stk
        | ')', '(' :: stk -> stk
        | ']', '[' :: stk -> stk
        | '}', '{' :: stk -> stk
        | '>', '<' :: stk -> stk
        | _, _ -> raise (Illegal c)
      ) [] line
    in Ok stk
  with Illegal c -> Error c

let part1 () =
  List.map (fun line ->
    match eval line with Ok _ -> 0 | Error c -> score c
  ) data
  |> List.fold_left (+) 0
  |> Printf.printf "%d\n"

let () = part1 ()

let score = function
  | '(' -> 1
  | '[' -> 2
  | '{' -> 3
  | '<' -> 4
  | _ -> assert false

let part2 () =
  data
  |> List.filter_map Fun.(eval %> Result.to_opt)
  |> List.map (List.fold_left (fun n c -> n * 5 + score c) 0)
  |> List.sort compare
  |> (fun l -> List.drop (List.length l / 2) l)
  |> List.hd
  |> Printf.printf "%d\n"

let () = part2 ()
