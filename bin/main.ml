let file = "1.in"

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

let () = printf "Pt.1 Sum: %d\n" sum

(* Day 1 Pt. 2 *)
type map_t = (string, int, Base.String.comparator_witness) Map_intf.Map.t
let string_map : map_t = Map.of_alist_exn (module String) [
  ("one", 1); ("two", 2); ("three", 3); ("four", 4); ("five", 5); ("six", 6); ("seven", 7); ("eight", 8); ("nine", 9);
  ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)
]

(* Given non-empty src, non-empty window length -> get list of all substrings (with start idx) of that length - list could be empty *)
let get_all_substrings (src:string) (len:int) : (int * string) list = 
  let src_len = String.length src in
  if src_len < len then [] else
  let aux (idx: int) : (int * string) = 
    (idx, String.sub src ~pos:idx ~len:len)
  in 
  List.init (src_len - len + 1) ~f:(fun x -> x)
  |> List.map ~f: aux

(* Non-empty src, non-empty pattern -> list of (idx, substring) tuples matching pattern *)
let process_key (src: string) (pattern:string) : (int * string) list = 
  get_all_substrings src (String.length pattern) 
  |> List.filter ~f: (fun (_, sub) -> String.equal sub pattern)

(* Given src, get all (idx, substring) tuples corresponding to string_map *)
let process_line (src:string) (string_map: map_t) : int = 
  let compare_tup (first: (int * 'a)) (snd: (int * 'b)) : int = 
    let idx1,_ = first in 
    let idx2,_ = snd in
    Int.compare idx1 idx2
  in
  Map.keys string_map 
  |> List.map ~f: (fun sub -> process_key src sub)
  |> List.concat
  |> List.sort ~compare: compare_tup
  |> List.map ~f: (fun (_, sub) -> Option.value ~default: 0 (Map.find string_map sub))
  |> list_to_number

let sum2 = lines 
|> List.map ~f: (fun line -> process_line line string_map) 
|> List.fold ~init:0 ~f:(+);;

printf "Pt.2 Sum: %d\n" sum2
