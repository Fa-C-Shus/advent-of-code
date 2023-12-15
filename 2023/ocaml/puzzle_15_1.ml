open Core

let rec get_sub_hash acc input =
  match String.length input with
  | 0 -> acc
  | _ -> 
    let char = String.get input 0 in
    let acc' = ((acc + (Char.to_int char)) * 17) % 256 in
    (* printf "char: %c, acc: %d, new_acc: %d\n" char acc acc'; *)
    get_sub_hash acc' (String.sub input ~pos:1 ~len:((String.length input) - 1))
;;

let rec process_cmds acc cmds =
  match cmds with
  | [] -> acc
  | cmd :: cmds -> 
    let new_acc = (acc + (get_sub_hash 0 cmd)) in
    (* printf "cmd: %s, acc: %d, new_acc: %d\n" cmd acc new_acc; *)
    process_cmds new_acc cmds
;;

let rec get_hash acc =
  match In_channel.input_line In_channel.stdin with
  | Some line -> 
    (* Split the line on ',' to create a list of substrings *)
    let cmds = String.split line ~on:',' in
    let new_acc = (acc + (process_cmds 0 cmds)) in
    (* printf "line: %s, acc: %d, new_acc: %d\n" line acc new_acc; *)
    get_hash new_acc
  | None -> acc
;;

let () = 
  match get_hash 0 with
  | 0 -> printf "Whiskey Tango Firetruck\n"
  | hash -> printf "15.1: %d\n" hash
;;
  (*
  dune build puzzle_15_1.exe && cat ../data/day-15-test | dune exec ./puzzle_15_1.exe  
  *)