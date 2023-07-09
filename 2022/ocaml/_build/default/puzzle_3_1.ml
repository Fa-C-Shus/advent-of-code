open Stdio

module CharSet = Set.Make(Char)

let string_to_char_set (str : string) : CharSet.t =
  let rec build_set_from_string index set =
    if index < 0 then set
    else build_set_from_string (index - 1) (CharSet.add (String.get str index) set)
  in
  build_set_from_string (String.length str - 1) CharSet.empty
;;

let halve input =
  let len = String.length input in
  let half = len / 2 in
  let first_half = String.sub input 0 half in
  let second_half = String.sub input half half in
  (first_half, second_half)

let get_priority ch =
  let ord = Char.code ch in 
  match ord with 
  | _ when ord <= Char.code 'Z' -> ord - Char.code 'A' + 27
  | _ when ord <= Char.code 'z' -> ord - Char.code 'a' + 1
  | _ -> 0
;;

let rec sum_priority accum = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> 
    let parts = halve line in
    let set1 = string_to_char_set (parts |> fst) in 
    let set2 = string_to_char_set (parts |> snd) in 
    let shared_char = CharSet.choose (CharSet.inter set1 set2) in
    (* printf "%c => %d\n" shared_char (get_priority shared_char); *)
    sum_priority (accum + get_priority shared_char)
  | None -> accum
;;

let () = 
  match sum_priority 0 with
  | 0 -> printf "No rucksacks\n"
  | calories -> printf "Priority Score: %d\n" calories

  (*
  dune build puzzle_3_1.exe && cat ../data/day-3 | dune exec ./puzzle_3_1.exe   
  *)