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

let rec find_start_packet signal prev index =
  let set = string_to_char_set prev in
  (* printf "signal: %s, prev: %s, index: %d\n" (String.sub signal 0 1) prev index; *)
  match (CharSet.cardinal set) with
  | 14 -> index
  | _ -> 
    let f_char = String.sub signal 0 1 in
    (* printf "\tf_char: %s\n" f_char; *)
    let prev = (match (String.length prev) with 
      | 14 -> (String.sub prev 1 13) ^ f_char
      | _ -> prev ^ f_char
    ) in
    (* printf "\tprev: %s\n" prev; *)
    let signal = String.sub signal 1 ((String.length signal) - 1) in
    find_start_packet signal prev (index + 1);
;;

let _ = 
  match find_start_packet (List.hd data) "" 0 with
  | index -> printf "index: %d\n" index


(*
dune build puzzle_6_2.exe && dune exec ./puzzle_6_2.exe   
*)