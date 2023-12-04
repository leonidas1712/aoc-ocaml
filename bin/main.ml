open Core
open One

let file = "1.in"
let lines = In_channel.read_lines file

(* Day 1 Pt. 1 *)
let sum = lines 
|> List.map ~f: string_to_num 
|> List.fold ~init:0 ~f:(+) 

let () = printf "Pt.1 Sum: %d\n" sum

(* Day 1 Pt. 2 *)
let sum2 = lines 
|> List.map ~f: (fun line -> process_line line string_map) 
|> List.fold ~init:0 ~f:(+);;

printf "Pt.2 Sum: %d\n" sum2;