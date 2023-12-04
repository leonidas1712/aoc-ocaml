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


(* function that takes a string line and outputs a 2-tuple of the nums (duplicate if need) *)
(* let string_to_nums (str: string) : (int * int) = 
  (1,2) *)
