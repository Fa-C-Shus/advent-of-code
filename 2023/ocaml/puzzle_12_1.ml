open Core

module IntTriple = struct
  type t = int * int * int
  let compare (x1, y1, z1) (x2, y2, z2) =
    match compare x1 x2 with
    | 0 -> (match compare y1 y2 with
            | 0 -> compare z1 z2
            | c -> c)
    | c -> c

  let t_of_sexp sexp = 
    (* Define conversion from S-expression to t *)
    (* This is a placeholder implementation *)
    let open Core.Sexp in
    match sexp with
    | List [Atom x; Atom y; Atom z] -> (int_of_string x, int_of_string y, int_of_string z)
    | _ -> failwith "Invalid S-expression for IntTriple"

  let sexp_of_t (x, y, z) = 
    (* Define conversion from t to S-expression *)
    Core.Sexp.List [Core.Sexp.Atom (string_of_int x); Core.Sexp.Atom (string_of_int y); Core.Sexp.Atom (string_of_int z)]
end

module IntTripleMap = Core.Map.Make(IntTriple)

let rec get_data data = 
  match In_channel.input_line In_channel.stdin with
  | None -> data
  | Some line -> 
    get_data (data @ [line])
  ;;

(* let count_combinations s =
  let count = String.fold s ~init:0 ~f:(fun acc c -> if (Char.equal c '?') then acc + 1 else acc) in
  int_of_float (2. ** float_of_int count)
;; *)

let get_current_keys s =
  let rec get_current_keys' count acc idx =
    if idx >= String.length s then
      if count > 0 then List.rev (count :: acc) else List.rev acc
    else if Char.equal (String.get s idx) '?' then
      if count > 0 then List.rev (count :: acc) else List.rev acc
    else if Char.equal (String.get s idx) '#' then
      get_current_keys' (count + 1) acc (idx + 1)
    else
      if count > 0 then get_current_keys' 0 (count :: acc) (idx + 1)
      else get_current_keys' 0 acc (idx + 1)
  in
  get_current_keys' 0 [] 0
;;

let is_valid parts keys =
  let rec is_valid' parts keys part_idx hash_len found =
    if String.length parts = part_idx then
      if hash_len = 0 then
        (List.equal Int.equal keys found)
      else
        let found' = (found @ [hash_len]) in
        (List.equal Int.equal keys found')
    else
      let current_part = String.get parts part_idx in
      match current_part with
      | '.' ->
          if hash_len = 0 then
            is_valid' parts keys (part_idx + 1) 0 found
          else
            is_valid' parts keys (part_idx + 1) 0 (found @ [hash_len])
      | '#' ->
          is_valid' parts keys (part_idx + 1) (hash_len + 1) found
      | _ -> 
          Printf.printf "Invalid char: %c in parts.\n" current_part;
          false
  in
  is_valid' parts keys 0 0 []
;;

let still_valid test_list master_list =
  let rec still_valid' test master idx = 
    (* printf "idx: %d\n" idx; *)
    (* printf "test  : %s\n" (List.to_string ~f:Int.to_string test);
    printf "master: %s\n\n" (List.to_string ~f:Int.to_string master); *)
    match test, master with
    | [], _ -> true
    | _, [] -> 
      printf "master is empty\n";
      false
    | _, _ ->
      (match idx with
      | 0 -> 
        let test_element = List.nth_exn test idx in
        let master_element = List.nth_exn master idx in
        (match (test_element <= master_element) with
        | true -> 
          (* keep going *)
          still_valid' test master (idx + 1)
        | false -> 
          (* printf "test_element: %d, > master_element: %d\n" test_element master_element; *)
          false)
      | _ when idx > (List.length test) - 1 -> true
      | _ ->
        let test_element = List.nth_exn test idx in
        let prev_test_element = List.nth_exn test (idx - 1) in
        let master_element = List.nth_exn master idx in
        let prev_master_element = List.nth_exn master (idx - 1) in
        (match (test_element <= master_element && prev_test_element = prev_master_element) with
        | true -> 
          (* keep going *)
          still_valid' test master (idx + 1)
        | false -> 
          (* printf "test_element: %d, > master_element: %d\n" test_element master_element; *)
          false))
  in
  List.length test_list <= List.length master_list && still_valid' test_list master_list 0
;;

(* let still_valid test_list master_list =
  let rec still_valid' test master = 
    match test, master with
    | [], _ -> true
    | _, [] -> false
    | [x1; x2], [y1; y2] -> 
      printf "x1: %d, x2: %d, y1: %d, y2: %d\n" x1 x2 y1 y2;
      x1 = y1 && x2 <= y2
    | xh :: xt, yh :: yt -> 
      printf "xh: %d, yh: %d\n" xh yh;
      xh <= yh && still_valid' xt yt
  in
  List.length test_list <= List.length master_list && still_valid' test_list master_list
;; *)

let find_valid_patterns parts keys p_idx k_idx

let rec count_true_results combinations keys acc =
  match combinations with
  | [] -> acc
  | hd :: tl -> 
      if is_valid hd keys then
        count_true_results tl keys (acc + 1)
      else
        count_true_results tl keys acc
  ;;

let rec generate_combinations s keys =
  let rec find_question_mark s idx =
    if idx >= String.length s then
      None
    else if Char.equal (String.get s idx) '?' then
      Some idx
    else
      find_question_mark s (idx + 1)
  in
  match find_question_mark s 0 with
  | Some idx ->
      let replace idx ch = 
        (* printf "replace: %d, %c, %s\n" idx ch s; *)
        String.mapi ~f:(fun i c -> if i = idx then ch else c) s
      in
      (* printf "replace: %s\n" (replace idx '.'); *)
      let current_keys = get_current_keys s in
      (match still_valid current_keys keys with
      | false -> []
      | true ->
        (* printf "result = %s\n" (List.to_string ~f:Int.to_string current_keys); *)
        let dot_path = generate_combinations (replace idx '.') keys in
        let hash_path = generate_combinations (replace idx '#') keys in
        List.concat [dot_path; hash_path])
  | None -> [s]
    ;;

let rec replicate input times sep output =
  match times with
  | 0 -> output
  | 1 -> output ^ sep ^ input
  | _ -> replicate input (times - 1) sep (output ^ sep ^ input )

let rec sum_combos acc folds data = 
  match data with
  | [] -> acc
  | hd :: tl -> 
      (* printf "input: %s\n" hd; *)
      let (parts, keys) = 
        match (String.lsplit2 hd ~on:' ') with
        | None -> failwith "No split"
        | Some (p, k) -> 
          ((replicate p folds "?" p), (replicate k folds "," k) |> String.split ~on:',' |> List.map ~f:(fun x -> int_of_string x )) in
      printf "parts: %s\n" parts;
          (* printf "parts: %s\n" parts;
      printf "keys: %d\n" (List.length keys);
      printf "is_valid: %b\n" (is_valid parts keys); *)
      (* printf "There could be %d possible combinations.\n" (count_combinations parts); *)
      let combinations = generate_combinations parts keys in
      (* printf "We generated %d combinations.\n" (List.length combinations); *)
      (* let () = List.iter combinations ~f:(fun x -> printf "%s\n" x) in *)
      let valid_count = count_true_results combinations keys 0 in
      
      sum_combos (acc + valid_count) folds tl
  ;;

let () = 
  let data = get_data [] in
  (* let first_puzzle = match sum_combos 0 (1-1) data with
    | 0 -> "You're Lost\n"
    | combos -> Printf.sprintf "12.1: %d\n" combos in
  Printf.printf "%s" first_puzzle; *)

  let second_puzzle = match sum_combos 0 (5-1) data with
    | 0 -> "You're Lost\n"
    | combos -> Printf.sprintf "12.2: %d\n" combos in
  Printf.printf "%s" second_puzzle;
  ;;
  (*
  dune build puzzle_12_1.exe && cat ../data/day-12-test | dune exec ./puzzle_12_1.exe  
  *)

