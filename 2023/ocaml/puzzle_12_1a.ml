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

let find_valid_patterns parts keys = 
  let cache = ref IntTripleMap.empty in
  let rec find_valid_patterns' acc parts keys p_idx k_idx =
    (* printf "params: p_idx: %d\tn_idx: %d\ttotal: %d\n" p_idx k_idx acc; *)
    let cache_key = (p_idx, k_idx, 0) in
    let key_len = List.length keys in
    (* printf "key_len: %d\n" key_len; *)
    let part_len = String.length parts in
    (* printf "part_len: %d\n" part_len; *)
    let curr_key = 
      match (k_idx < key_len) with
      | true -> List.nth_exn keys k_idx
      | false -> 0 
    in
    (* printf "curr_key: %d\n" curr_key; *)
    (* printf "parts: %s\n" parts; *)
    (* printf "cache length: %d\n" (Map.length !cache); *)
    (* printf "cache_key: %s\n" (IntTriple.sexp_of_t cache_key |> Sexp.to_string_hum); *)
    match Map.find !cache cache_key with
    (* Do we know this answer - first Base Case*)
    | Some value -> 
      (* printf "cache hit: %d or %d\n" value (acc + value); *)
      acc + value
    | None -> 
      (* printf "cache miss\n"; *)
      (* printf "p_idx: %d\tpart_len: %d\n" p_idx part_len;   *)
      match (p_idx > part_len) with
      | true -> 
        (* Have we reached the end of the string - second Base Case *)
        (* printf "end of string\n"; *)
        (match (k_idx = key_len) with
        | true -> 
          (* Have we used all the keys - third Base Case *)
          (* printf "\tused all keys\n"; *)
          acc + 1
        | false -> 
          (* Have we NOT used all the keys - third Base Case*)
          (* printf "\tnot used all keys\n"; *)
          acc)
      | false ->
        (* we may call find_valid_patterns' multiple times *)
        (* if char is '.' || '?' *)
        let current_part = 
          match (p_idx < part_len) with
          | true -> parts.[p_idx]
          | false -> '!'
        in
        (* printf "current_part: %c\n" current_part; *)
        let dot_q = 
          match ((Char.equal current_part '.') || (Char.equal current_part '?')) with
          | true -> 
            (* printf "we have a dot or question mark\n"; *)
            let acc' = find_valid_patterns' acc parts keys (p_idx + 1) k_idx in
            cache := Map.set !cache ~key:cache_key ~data:acc';
            acc' + acc
          | false when p_idx >= part_len -> 
            (* printf "we have a dot or question mark\n"; *)
            let acc' = find_valid_patterns' acc parts keys (p_idx + 1) k_idx in
            cache := Map.set !cache ~key:cache_key ~data:acc';
            acc' + acc
          | false ->
            acc
        in

        (* printf "dot_q: %d\n" dot_q; *)

        let pn_idx = p_idx + curr_key in
        (* printf "p_idx: %d\tpn_idx: %d\tcurr_key: %d\n" p_idx pn_idx curr_key; *)
       
        let pn_char = 
          match (part_len > pn_idx) with
          | true -> parts.[pn_idx]
          | false -> '.'
        in

        (* if char is '#' || '?' *)
        let hash_seg = 
          match (part_len >= p_idx + curr_key) with
          | true -> String.sub ~pos:p_idx ~len:curr_key parts
          | false -> "!" 
        in
        (* printf "Hash_seg: %s\tcc: %c\n" hash_seg current_part; *)

        (* printf "curr_part: %c\thash_seg: %s\tpn_char: %c\n" current_part hash_seg pn_char; *)
        let hash_q = 
          match (pn_idx > part_len) with
          | true -> 
            acc
          | false ->
            match ((Char.equal current_part '#') || (Char.equal current_part '?')) 
              && (not (String.contains hash_seg '.')) 
              && (not (Char.equal pn_char '#')) with
            | true -> 
              (* printf "we have a # or question mark\n"; *)
              let acc' = find_valid_patterns' acc parts keys (pn_idx + 1) (k_idx + 1) in
              cache := Map.set !cache ~key:cache_key ~data:acc';
              acc' + acc
            | false -> 
              (* printf "we have a # or question mark but XXX\n"; *)
              acc
        in
        cache := Map.set !cache ~key:cache_key ~data:(dot_q + hash_q);
        (* printf "params: p_idx: %d\tn_idx: %d\ttotal: %d\n" p_idx k_idx (dot_q + hash_q); *)
        (dot_q + hash_q)
    
  in
  (* print all of the cached elements *)
  (* Map.iteri !cache ~f:(fun ~key ~data -> printf "key: %s\tdata: %d\n" (IntTriple.sexp_of_t key |> Sexp.to_string_hum) data); *)
  find_valid_patterns' 0 parts keys 0 0
  ;;

let rec replicate input times sep output =
  match times with
  | 0 -> output (* can't really get here *)
  | 1 -> output ^ sep ^ input
  | _ -> replicate input (times - 1) sep (output ^ sep ^ input )
;;

let sum_valid_patterns acc folds data =
  let rec sum_valid_patterns' acc folds data =
    match data with
    | [] -> acc
    | hd :: tl -> 
        (* printf "input: %s\n" hd; *)
        let (parts, keys) = 
          match (String.lsplit2 hd ~on:' ') with
          | None -> failwith "No split"
          | Some (p, k) -> 
            ((replicate p folds "?" p), (replicate k folds "," k) |> String.split ~on:',' |> List.map ~f:(fun x -> int_of_string x )) in
      let valid_patterns = find_valid_patterns parts keys in
      (* printf "parts: %s\tnum: %d\n" parts valid_patterns; *)
      sum_valid_patterns' (acc + valid_patterns) folds tl in
  sum_valid_patterns' acc folds data
;;

  let () = 
  let data = get_data [] in
  (* printf "data:\n \t%s\n" (String.concat ~sep:"\n\t" data); *)
  let first_puzzle = match sum_valid_patterns 0 (1-1) data with
    | 0 -> "You're Lost\n"
    | combos -> Printf.sprintf "12.1: %d\n" combos in
  Printf.printf "%s" first_puzzle;

  let second_puzzle = match sum_valid_patterns 0 (5-1) data with
    | 0 -> "You're Lost\n"
    | combos -> Printf.sprintf "12.2: %d\n" combos in
  Printf.printf "%s" second_puzzle;
  ;; 
  (*
  dune build puzzle_12_1a.exe && cat ../data/day-12-test | dune exec ./puzzle_12_1a.exe  
  *)