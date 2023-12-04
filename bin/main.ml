let file = "1_1.in"

open Core

(* int list to concatenated number (duplicate if needed) *)
  (* [1,2,3] -> 13, [7] -> 77 *)
let list_to_number (lst: int list) : int = 
  let last_idx = (List.length lst) - 1 in
  let hd = Option.value ~default:0 (List.hd lst) in
  let tl = Option.value ~default:0 (List.nth lst last_idx) in
  hd * 10  + tl


(* function that takes a string line and outputs first + last int concat as number *)
let string_to_num (str: string) : int = 
  str
  |> String.to_list
  |> List.map ~f:Char.get_digit
  |> List.filter_map ~f: (fun x -> x)
  |> list_to_number


let lines = In_channel.read_lines file

(* Day 1 Pt. 1 *)
let sum = lines 
|> List.map ~f: string_to_num 
|> List.fold ~init:0 ~f:(+) 

let () = printf "Sum : %d\n" sum

