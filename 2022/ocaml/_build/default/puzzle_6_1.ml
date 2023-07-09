open Stdio

module CharSet = Set.Make(Char)

let data = Helper.read_lines "../data/day-6"

let string_to_char_set (str : string) : CharSet.t =
  let rec build_set_from_string index set =
    if index < 0 then set
    else build_set_from_string (index - 1) (CharSet.add (String.get str index) set)
  in
  build_set_from_string (String.length str - 1) CharSet.empty
;;

let rec find_start_packet signal prev4 index =
  let set = string_to_char_set prev4 in
  (* printf "signal: %s, prev4: %s, index: %d\n" (String.sub signal 0 10) prev4 index; *)
  match (CharSet.cardinal set) with
  | 4 -> index
  | _ -> 
    let f_char = String.sub signal 0 1 in
    (* printf "\tf_char: %s\n" f_char; *)
    let prev4 = (match (String.length prev4) with 
      | 4 -> (String.sub prev4 1 3) ^ f_char
      | _ -> prev4 ^ f_char
    ) in
    (* printf "\tprev4: %s\n" prev4; *)
    let signal = String.sub signal 1 ((String.length signal) - 1) in
    find_start_packet signal prev4 (index + 1);
;;

let _ = 
  (* let yadda = List.hd data in 
    printf "yadda: %s\n" yadda; *)
  match find_start_packet (List.hd data) "" 0 with
  | index -> printf "index: %d\n" index


(*
dune build puzzle_6_1.exe && dune exec ./puzzle_6_1.exe   
*)