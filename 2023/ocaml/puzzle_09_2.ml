open Core

(* let rec predict_next_value int_list child_list = 
  match int_list with
  | [] -> 0
  | Some hd :: Some second :: tl  -> 
    let child_item = hd - second in
    let child_list' = List.append child_list [Some child_item] in
    predict_next_value (Some second::tl) child_list'
  | _ -> 
    printf "int_list: %d\n" (List.length int_list);
    printf "child_list: %d\n" (List.length child_list);
    predict_next_value int_list child_list
  ;; *)

  let rec display_list int_list ret_val = 
    match int_list with
    | [] -> ret_val;
    | hd :: tl -> 
      let ret_val' = ret_val ^ (Int.to_string hd) ^ ", " in
      display_list tl ret_val'
    ;;

  let rec is_all_zeros list = 
    match list with
    | [] -> true
    | hd :: tl -> 
      if hd = 0 then is_all_zeros tl
      else false
    ;;

  let rec patch_last_value list value =
    match list with
    | [] -> value
    | hd :: tl -> 
      printf "value: %d\thd: %d\n" value hd;
      let value' = hd - value in
      printf "patch_last_value: %d\n" value';
      patch_last_value tl value'
    ;;

  let rec predict_next_value parent_list child_list trail depth is_head =
    match parent_list with
    | hd :: second :: tl -> 
      let child_item = second - hd in
      let child_list' = child_list @ [child_item] in
      let trail' = 
        match (List.hd parent_list) with 
        | Some x when is_head -> x :: trail
        | _ -> trail
       in
      printf "Trail: %s\n" (display_list trail' "");
      let is_head' = false in 
      predict_next_value (second::tl) child_list' trail' depth is_head'
    | _ -> 
      (* I now have the child row; test for all 0 *)
      printf "->Parent: %s\n" (display_list parent_list "");
      printf "->Child: %s\n" (display_list child_list "");
      match (is_all_zeros child_list) with
      | true -> 
        printf "Zeroed child depth: %d with %s\n" depth (display_list trail "");
        patch_last_value trail 0
      | false ->
        printf "Go Again: %b\n" (is_all_zeros child_list);
        predict_next_value child_list [] trail (depth + 1) true
      ;;

let rec analyze_oasis acc = 
  match In_channel.input_line In_channel.stdin with
  | None -> acc
  | Some line -> 
    let int_list = Helper.string_to_int_list line in
    let prediction = predict_next_value int_list [] [] 1 true in
    (* let int_list = Helper.string_to_int_list line in
    List.iteri int_list ~f:(fun i x -> 
      printf "%d: %d\n" i x
    ); *)
    analyze_oasis (acc + prediction)
  ;;

let () = 
  match analyze_oasis 0 with
  | 0 -> printf "You're Lost\n"
  | score -> printf "9.1: %d\n" score

  (*
  dune build puzzle_09_2.exe && cat ../data/day-09-test | dune exec ./puzzle_09_2.exe   
  *)