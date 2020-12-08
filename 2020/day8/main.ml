open Containers

type instr =
  | Nop of int
  | Acc of int
  | Jmp of int

let instr_of (s, x) =
  match s with
  | "nop" -> Nop x
  | "acc" -> Acc x
  | "jmp" -> Jmp x
  | _ -> failwith ("unknown instr " ^ s)

let data =
  CCIO.(with_in Sys.argv.(1) read_lines_l)
  |> List.map (fun s -> Scanf.sscanf s "%s %d" (fun s x -> (s, x)))
  |> List.map instr_of

let code = Array.of_list data

let run code =
  let visited = Array.make (Array.length code) false in
  let rec loop pc acc =
    if pc >= Array.length code then `Halt acc
    else if visited.(pc) then `Loop acc
    else begin
      visited.(pc) <- true;
      match code.(pc) with
      | Nop _ -> loop (pc + 1) acc
      | Acc x -> loop (pc + 1) (acc + x)
      | Jmp x -> loop (pc + x) acc
      end
  in
  loop 0 0

let part1 () =
  match run code with
  | `Loop x -> Printf.printf "%d\n%!" x
  | _ -> assert false

let rec tryfix code pc =
  match code.(pc) with
  | Acc _ -> tryfix code (pc+1)
  | Nop x ->
     code.(pc) <- Jmp x;
     begin match run code with
     | `Halt x -> x
     | `Loop _ ->
       code.(pc) <- Nop x;
       tryfix code (pc+1)
     end
  | Jmp x ->
     code.(pc) <- Nop x;
     begin match run code with
     | `Halt x -> x
     | `Loop _ ->
       code.(pc) <- Jmp x;
       tryfix code (pc+1)
     end

let part2 () =
  Printf.printf "%d\n%!" (tryfix code 0)

let () = part2 ()
