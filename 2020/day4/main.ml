open Containers

let rec group acc = function
  | "" :: ll -> acc :: group [] ll
  | l :: ll -> group (l :: acc) ll
  | [] -> [acc]

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.flat_map (String.split_on_char ' ')
  |> group []
  |> List.map (List.map (fun s -> let [x;y] = String.split_on_char ':' s in (x, y)))

module SSet = Set.Make(String)

let fields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"] |> SSet.of_list
let fields_opt = ["cid"] |> SSet.of_list
let all_fields = SSet.union fields fields_opt

let check_fields l =
  let l = List.map fst l in
  let s = SSet.of_list l in
  SSet.subset fields s && SSet.subset s all_fields

let part1 () =
  data |> List.filter check_fields |> List.length

let validate (k, v) =
  try
    begin match k with
    | "byr" ->
       let i = int_of_string v in
       1920 <= i && i <= 2002
    | "iyr" ->
       let i = int_of_string v in
       2010 <= i && i <= 2020
    | "eyr" ->
       let i = int_of_string v in
       2020 <= i && i <= 2030
    | "hgt" ->
       begin
         try Scanf.sscanf v "%dcm" (fun x -> 150 <= x && x <= 193)
         with _ ->
           Scanf.sscanf v "%din" (fun x -> 59 <= x && x <= 76)
       end
    | "hcl" ->
       let l = String.to_list v in
       let char_in c1 c2 c = Char.code c1 <= Char.code c && Char.code c <= Char.code c2 in
       List.length l = 7 &&
       Char.equal (List.hd l) '#' &&
       List.for_all (fun c -> char_in '0' '9' c || char_in 'a' 'f' c) (List.tl l)
    | "ecl" ->
       List.mem ~eq:String.equal v ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
    | "pid" ->
       ignore (int_of_string v);
       String.length v = 9
    | _ -> true
    end
  with _ -> false

let part2 () =
  data |> List.filter check_fields |> List.filter (List.for_all validate) |> List.length

let () = Printf.printf "%d\n%!" (part2 ())
