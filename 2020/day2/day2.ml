open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s -> Scanf.sscanf s "%d-%d %c: %s" (fun a b c d -> (a, b, c, d)))

let part1 () =
  let is_valid (a, b, c, s) =
    let x = String.to_list s |> List.filter (Char.equal c) |> List.length in
    a <= x && x <= b
  in
  List.filter is_valid data |> List.length |> Printf.printf "%d\n%!"

let part2 () =
  let is_valid (a, b, c, s) =
    let y =
      String.to_seqi s |> OSeq.filter (fun (i, _) -> i+1 = a || i+1 = b)
      |> OSeq.filter (fun (_, x) -> Char.equal x c) in
    OSeq.length y = 1
  in
  List.filter is_valid data |> List.length |> Printf.printf "%d\n%!"

let () = part2 ()
