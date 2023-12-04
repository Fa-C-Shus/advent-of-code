open Core

module StringSet = Set.Make(String)
;;

let trim_spaces str =
  Str.global_replace (Str.regexp "^[ \t\n\r]+\\|[ \t\n\r]+$") "" str
;;

let split_and_trim input =
  let parts = Str.split (Str.regexp "|") input in
  List.map ~f:trim_spaces parts
;;

let split_string str = 
  Str.split (Str.regexp " +") str
;;

let rec pow a b =
  if b = 0 then 1
  else a * pow a (b - 1)
;;

let rec sum_prizes accum = 
  match In_channel.input_line In_channel.stdin with
  | Some "" -> accum (* End of elf *)
  | Some line -> 
    (* printf "Line: %s\n" line; *)

    (* Remove "Card #:" part *)
    let no_lable_line = Str.global_replace (Str.regexp "Card [0-9]+: ") "" line in

    (* Split by pipe *)
    let parts = split_and_trim no_lable_line in

    let (winning_numbers, my_numbers) = 
      match parts with
      | [a; b] -> (a, b)
      | _ -> failwith "List does not contain exactly two elements" in

    let winning_set = StringSet.of_list (split_string winning_numbers) in
    let my_set = StringSet.of_list (split_string my_numbers) in
    (* printf "Winning set: %d\n" (Set.length winning_set);
    printf "My set: %d\n" (Set.length my_set); *)

    let matching_set = Set.inter winning_set my_set in
    let matches = Set.length matching_set in
    let prize = 
      match matches with
      | 0 -> 0
      | _ -> pow 2 (matches - 1) in

      sum_prizes (accum + prize)
  | None -> accum

let () = 
  match sum_prizes 0 with
  | 0 -> printf "No soup for you\n"
  | calibrations -> printf "Day 4.1: %d\n" calibrations


  (*
  dune build puzzle_04_1.exe && cat ../data/day-04-test | dune exec ./puzzle_04_1.exe  
  *)