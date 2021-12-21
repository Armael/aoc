open Containers

type ddie_st = { mutable n: int; mutable count: int }
let mk_ddie () = { n = 0; count = 0 }

let roll st =
  if st.n = 100 then st.n <- 1 else st.n <- st.n + 1;
  st.count <- st.count + 1;
  st.n

type player = { mutable pos: int; mutable score: int }

let turn die player =
  let res = roll die + roll die + roll die in
  player.pos <- ((player.pos - 1) + res) mod 10 + 1;
  player.score <- player.score + player.pos

let part1 p1 p2 =
  let die = mk_ddie () in
  let rec loop p1 p2 =
    turn die p1;
    if p1.score >= 1000 then p2.score * die.count
    else loop p2 p1
  in
  loop { pos = p1; score = 0 } { pos = p2; score = 0 }

let () = part1 7 2 |> Printf.printf "%d\n"

(*****)

type player' = { pos: int; score: int }

let (let*) l f = List.flat_map f l
let return x = [x]
let roll = [1;2;3]

let turn (p:player') =
  let* d1 = roll in
  let* d2 = roll in
  let* d3 = roll in
  let res = d1 + d2 + d3 in
  let pos = ((p.pos - 1) + res) mod 10 + 1 in
  return { pos; score = p.score + pos }

let h = Hashtbl.create 4000

let rec loop p1_playing (p1, p2) =
  match Hashtbl.find h (p1_playing, p1, p2) with
  | (w1, w2) -> (w1, w2)
  | exception Not_found ->
    if p1.score >= 21 then (
      Hashtbl.replace h (p1_playing, p1, p2) (1, 0); (1, 0)
    ) else if p2.score >= 21 then (
      assert p1_playing; (* p2 just played *)
      Hashtbl.replace h (p1_playing, p1, p2) (0, 1); (0, 1)
    ) else (
      let p = if p1_playing then p1 else p2 in
      let ps' = turn p in
      let ws =
        List.map (fun p' ->
          let (p1', p2') = if p1_playing then (p', p2) else (p1, p') in
          loop (not p1_playing) (p1', p2')) ps'
        |> List.reduce (fun (a,b) (c,d) -> (a+c,b+d))
        |> Option.get_exn_or "ono" in
      Hashtbl.replace h (p1_playing, p1, p2) ws;
      ws
    )

let part2 p1 p2 =
  let w1, w2 = loop true ({ pos = p1; score = 0 }, { pos = p2; score = 0 }) in
  max w1 w2

let () = part2 7 2 |> Printf.printf "%d\n"
