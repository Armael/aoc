open Containers
let (=) = Stdlib.(=)

type move = R | P | S
let move_of_char = function
  | 'A' | 'X' -> R
  | 'B' | 'Y' -> P
  | 'C' | 'Z' -> S
  | _ -> assert false
let moves = [S;R;P]

type outcome = Win | Loss | Draw
let outcome = function
  | R, S | S, P | P, R -> Loss
  | S, R | P, S | R, P -> Win
  | R, R | P, P | S, S -> Draw
let outcome_score = function
  | Win -> 6 | Draw -> 3 | Loss -> 0
let outcome_of_char = function
  | 'X' -> Loss
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> assert false

let shape_score = function
  | R -> 1 | P -> 2 | S -> 3

let score (opp, me) =
  shape_score me + outcome_score (outcome (opp, me))

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s -> Scanf.sscanf s "%c %c" (fun a b -> (a, b)))

let part1 () =
  data
  |> List.map (fun (a, b) -> score (move_of_char a, move_of_char b))
  |> List.fold_left (+) 0
  |> Printf.printf "%d\n"

let part2 () =
  data
  |> List.map (fun (a, b) -> (move_of_char a, outcome_of_char b))
  |> List.map (fun (opp, out) -> (opp, List.find (fun m -> outcome (opp, m) = out) moves))
  |> List.map score
  |> List.fold_left (+) 0
  |> Printf.printf "%d\n"

let () =
  part2 ()
