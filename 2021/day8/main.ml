open Containers

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s -> Scanf.sscanf s "%s %s %s %s %s %s %s %s %s %s | %s %s %s %s"
                 (fun x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 o1 o2 o3 o4 ->
                    [x0; x1; x2; x3; x4; x5; x6; x7; x8; x9],
                    [o1; o2; o3; o4]))

let part1 () =
  data |> List.flat_map (fun (_, o) ->
    List.filter (fun s ->
      let n = String.length s in
      n = 2 || n = 4 || n = 3 || n = 7
    ) o
  )
  |> List.length
  |> Printf.printf "%d\n"

(* let () = part1 () *)

let part2 () =
  let perms = OSeq.memoize (OSeq.permutations [0; 1; 2; 3; 4; 5; 6] |> OSeq.map Array.of_list) in
  let eval_perm (p: int array) (s: string) =
    let a = Array.make 7 false in
    String.iter (fun c -> a.(p.(Char.code c - Char.code 'a')) <- true) s;
    match a with
    | [|true; true; true; false; true; true; true|] -> Some 0
    | [|false; false; true; false; false; true; false|] -> Some 1
    | [|true; false; true; true; true; false; true|] -> Some 2
    | [|true; false; true; true; false; true; true|] -> Some 3
    | [|false; true; true; true; false; true; false|] -> Some 4
    | [|true; true; false; true; false; true; true|] -> Some 5
    | [|true; true; false; true; true; true; true|] -> Some 6
    | [|true; false; true; false; false; true; false|] -> Some 7
    | [|true; true; true; true; true; true; true|] -> Some 8
    | [|true; true; true; true; false; true; true|] -> Some 9
    | _ -> None
  in
  List.map (fun (is, os) ->
    let p =
      OSeq.find (fun p ->
        List.filter_map (eval_perm p) is
        |> List.sort compare
        |> Stdlib.(=) [0;1;2;3;4;5;6;7;8;9]
      ) perms |> Option.get_exn_or "oh no"
    in
    List.map (eval_perm p) os
    |> List.map (Option.get_exn_or "oh no")
    |> List.rev
    |> List.fold_left (fun (acc, pow) n -> (acc + n * pow, pow * 10)) (0, 1)
    |> fst
  ) data
  |> List.fold_left (+) 0
  |> Printf.printf "%d\n"

let () = part2 ()
