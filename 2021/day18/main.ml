open Containers

type num =
  | Int of int
  | Pair of num * num

include struct
  open Angstrom

  let is_digit c =
    let n = Char.code c in
    n >= Char.code '0' && n <= Char.code '9'

  let parse_num : num t =
    fix (fun parse_num ->
      (satisfy is_digit >>| fun c -> Int (Char.code c - Char.code '0')) <|>
      (both (char '[' *> parse_num <* char ',') (parse_num <* char ']') >>| fun (n1, n2) -> Pair (n1, n2)))
end

let num_of_string s =
  match Angstrom.(parse_string ~consume:Consume.All parse_num s) with
  | Ok num -> num
  | Error err -> failwith ("'" ^ s ^ "'" ^ err)

let rec string_of_num = function
  | Int n -> string_of_int n
  | Pair (n1, n2) -> "[" ^ string_of_num n1 ^ "," ^ string_of_num n2 ^ "]"

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map num_of_string

type dir = L|R
type path = dir list

let string_of_dir = function
  | L -> "L"
  | R -> "R"

let rec string_of_path = function
  | [] -> ""
  | L::p -> "L " ^ string_of_path p
  | R::p -> "R " ^ string_of_path p

let rec depth = function
  | Int _ -> 0
  | Pair (n1, n2) -> 1 + max (depth n1) (depth n2)

let find_action num =
  let rec loop path = function
    | Int n ->
      if n >= 10 then Some (`Split, List.rev path) else None
    | Pair (n1, n2) ->
      assert (List.length path <= 4);
      if List.length path = 4 then
        Some (`Explode, List.rev path)
      else
        match loop (L::path) n1, loop (R::path) n2 with
        | Some (`Explode, p), _ -> Some (`Explode, p)
        | Some (`Split, _), Some (`Explode, p) -> Some (`Explode, p)
        | Some (`Split, p), _ -> Some (`Split, p)
        | None, res -> res
  in
  loop [] num

let rec find_neighbour ?(depth=0) p num dir =
  (* Printf.printf "%sfind_neighbour <%s> %s %s\n"
   *   (String.make depth '>') *)
    (* (string_of_path p) (string_of_num num) (string_of_dir dir); *)
  let p' =
  match p with
  | [] -> assert false
  | [d] ->
    begin match num with
    | Int _ -> assert false
    | Pair (n1, n2) ->
      match d, dir with
      | L, R -> Some (R::find_dir_path L n2)
      | R, L -> Some (L::find_dir_path R n1)
      | _, _ -> None
    end
  | d::ds ->
    begin match num with
      | Int _ -> assert false
      | Pair (n1, n2) ->
        match d with
        | L ->
          begin match find_neighbour ~depth:(succ depth) ds n1 dir with
            | Some p' -> Some (d::p')
            | None ->
              match dir with
              | L -> None
              | R -> Some (R::find_dir_path L n2)
          end
        | R ->
          begin match find_neighbour ~depth:(succ depth) ds n2 dir with
            | Some p' -> Some (d::p')
            | None ->
              match dir with
              | R -> None
              | L -> Some (L::find_dir_path R n1)
          end
    end
  in
  (* Printf.printf "%s -> <%s>\n" (String.make depth '>')
   *   (match p' with Some p' -> string_of_path p' | None -> "None"); *)
  p'

and find_dir_path dir num =
  let rec loop p = function
    | Int _ -> List.rev p
    | Pair (n1, n2) ->
      match dir with
      | L -> loop (L::p) n1
      | R -> loop (R::p) n2
  in loop [] num

let rec map_path f p num =
  match p, num with
  | [], num -> f num
  | d::ds, Pair (n1, n2) ->
    begin match d with
      | L -> Pair (map_path f ds n1, n2)
      | R -> Pair (n1, map_path f ds n2)
    end
  | _, _ -> assert false

let rec get_path p num =
  match p, num with
  | [], n -> n
  | d::ds, Pair (n1, n2) ->
    begin match d with
      | L -> get_path ds n1
      | R -> get_path ds n2
    end
  | _, _ -> assert false

let explode p num =
  let n1, n2 = match get_path p num with
    | Pair (Int n1, Int n2) -> n1, n2
    | _ -> assert false
  in
  let incr n = function
    | Int m -> Int (n+m)
    | _ -> assert false
  in
  let num = match find_neighbour p num L with
    | Some pl -> map_path (incr n1) pl num
    | None -> num in
  let num = match find_neighbour p num R with
    | Some pr -> map_path (incr n2) pr num
    | None -> num in
  map_path (fun _ -> Int 0) p num

let split p num =
  let n = match get_path p num with
    | Int n -> n
    | _ -> assert false
  in
  assert (n >= 10);
  let num = map_path (fun _ -> Pair (Int (n/2), Int (n/2 + n mod 2))) p num in
  assert (List.length p <= 4);
  num

let reduce2 num =
  assert (depth num <= 5);
  let rec loop num =
    (* Printf.printf "==== %s ====\n\n" (string_of_num num); *)
    match find_action num with
    | None -> num
    | Some (`Explode, p) ->
      (* Printf.printf "explode %s\n" (string_of_path p); *)
      let num = explode p num in
      loop num
    | Some (`Split, p) ->
      (* Printf.printf "split %s (%s)\n" (string_of_path p) (string_of_num (get_path p num)); *)
      let num = split p num in
      loop num
  in
  let res = loop num in
  assert (depth res < 5);
  res

let add_num num1 num2 =
  assert (depth num1 < 5);
  assert (depth num2 < 5);
  reduce2 (Pair (num1, num2))

let rec magnitude = function
  | Int n -> n
  | Pair (n1, n2) ->
    3 * magnitude n1 + 2 * magnitude n2

let () =
  let num =
    match data with
    | [x] -> reduce2 x
    | _ -> List.fold_left add_num (List.hd data) (List.tl data)
  in
  print_endline (string_of_num num);
  Printf.printf "%d\n" (magnitude num)

let () =
  List.product (fun n1 n2 -> if Stdlib.(n1 = n2) then 0 else magnitude (add_num n1 n2)) data data
  |> OSeq.of_list |> OSeq.max ~lt:(<)
  |> Printf.printf "%d\n"
