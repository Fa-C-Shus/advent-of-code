open Core

module StringSet = Set.Make(String)
;;

module CarryOver = struct
  type t = {
    card: int;
    copies: int;
  }

  (* let empty = {
    card = -1;  
    copies = 0;
  }

  let copywith ?(card=None) ?(copies=None) co =
    {
      card = Option.value card ~default:co.card;
      copies = Option.value copies ~default:co.copies;
    }
  let is_empty co = 
    co.card = -1   *)
end
let rec find_opt f lst =
  match lst with
  | [] -> None
  | hd :: tl -> if f hd then Some hd else find_opt f tl
;;

let update_or_add_carryover carryover_list card =
  match find_opt (fun co -> co.CarryOver.card = card) carryover_list with
  | Some co ->
      printf "Found carryover for card %d\t#:%d\n" card co.CarryOver.copies;
      let updated_co = {co with CarryOver.copies = co.CarryOver.copies + 1} in
      List.map ~f:(fun item -> if item.CarryOver.card = card then updated_co else item) carryover_list
  | None ->
      {CarryOver.card = card; copies = 1} :: carryover_list
  ;;

let apply_carryover_updates carryover_list index matches =
  let rec update_list carryover_list current_index remaining_matches =
    if remaining_matches > 0 then
      let updated_list = update_or_add_carryover carryover_list current_index in
      update_list updated_list (current_index + 1) (remaining_matches - 1)
    else
      carryover_list
  in
  update_list carryover_list (index + 1) matches
;;

let rec apply_carryover_updates_n_times carryover index matches n =
  match n with
  | 0 -> carryover
  | _ ->
    let updated_list = apply_carryover_updates carryover index matches in
    apply_carryover_updates_n_times updated_list index matches (n - 1)
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

let rec sum_prizes index accum carryover = 
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
    (* process original *)
    printf "Card %d: %d\n" index matches;

    (* process original *)
    let orig_prize = 1 in (* original is a copy *)

    (* process copies; if this index has copies *)
    let card_copies = 
      List.fold_left 
        ~init:0 
        ~f:(fun accum co -> 
              if co.CarryOver.card = index then accum + co.CarryOver.copies 
              else accum) 
        carryover in
    let copies_prize = orig_prize + card_copies in
    (* add a carryover for the copies *)
    let carryover' = apply_carryover_updates_n_times carryover index matches card_copies in
     
    printf "..Copies: %d\n" card_copies; 
    (* update the carryover copies data, For each match; add 1 to the carryover of the next matches card *)
    let carryover'' = apply_carryover_updates carryover' index matches in
    printf "..Carryover: %d\n" (List.length carryover''); 

    sum_prizes (index + 1) (accum + copies_prize) carryover''
  | None -> accum (* End of file *)

let () = 
  match sum_prizes 1 0 [] with
  | 0 -> printf "No soup for you\n"
  | calibrations -> printf "Day 4.2: %d\n" calibrations


  (*
  dune build puzzle_04_2.exe && cat ../data/day-04-test | dune exec ./puzzle_04_2.exe  
  *)