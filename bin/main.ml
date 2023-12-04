(* let () = print_endline "Hello, World!";
Printf.printf "String:%s %d\n" "Hi" 2;; *)
let file = "1_1.in"

open Core
(* let contents = In_channel.read_all file *)
let lines = In_channel.read_lines file

let fst = List.nth lines 0;;
match fst with 
| Some v -> print_endline v
| None -> ()


(* function that takes a string line and outputs list of nums inside (length 1 or 2 only) *)
let string_to_nums (str: string) : int list = 
  str
  |> String.to_list
  |> List.map ~f:Char.get_digit
  |> List.filter_map ~f: (fun x -> x)


  

