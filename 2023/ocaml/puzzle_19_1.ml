open Printf 

let parse_state_rule workshops line =
  let short_string = String.sub line 0 (String.length line - 1) in
  let index = String.index short_string '{' in
  let (name, remainder) = 
    (String.sub short_string 0 index), (String.sub short_string (index + 1) (String.length short_string - index - 1)) in
  (* now split the remainder on "," *)
  let rules' = Str.split (Str.regexp ",") remainder in
  let (fallback, rules) = 
    match List.rev rules' with
    | [] -> ("", rules')
    | hd :: tl -> 
      (* remove this element from the list of rules '*)
      (hd, tl)
  in
  (* printf "Fallback: %s\n" fallback; *)
  Hashtbl.add workshops name ([], fallback);
  (* printf "Rules: %d\n" (List.length rules);   *)
  let rec process_rules rules workshops =
    match rules with
    | [] -> workshops
    | rule :: rest ->
      (match Str.split (Str.regexp ":") rule with
      | [comparison; target] ->
        let key = String.sub comparison 0 1 in
        let cmp = String.sub comparison 1 1 in
        let value = int_of_string (String.sub comparison 2 (String.length comparison - 2)) in
        let entry = Hashtbl.find workshops name in
        let entry' = ((key, cmp, value, target) :: (fst entry), (snd entry)) in
        Hashtbl.replace workshops name entry';
        process_rules rest workshops
      | _ -> failwith "Invalid rule")
  in
  process_rules rules workshops
;;

let accept workflows item name =
  let rec accept' item name =
    match name with
    | "R" -> 
      (* reject the item *)
      false
    | "A" -> 
      (* accept the item *)
      true
    | _ -> 
      (* check the rules *)
      let (rules, fallback) = Hashtbl.find workflows name in
      (* List.iter (fun (key, cmp, value, target) -> printf "(%s, %s, %d, %s), " key cmp value target) rules;
      printf "Fallback: %s\n" fallback; *)
      let rec process_rules rules =
        match rules with
        | [] -> accept' item fallback
        | (key, cmp, value, target) :: rest ->
          match cmp with
          | "<" -> 
            let value' = Hashtbl.find item key in
            if value' < value then accept' item target else process_rules rest
          | ">" ->
            let value' = Hashtbl.find item key in
            if value' > value then accept' item target else process_rules rest
          | _ -> failwith "Invalid comparison"
      in
      process_rules rules
  in
  accept' item name
;;

let process_items workshops =
  (* printf "workshops: %d\n" (Hashtbl.length workshops); *)
  let rec process_items' acc  =
    match In_channel.input_line In_channel.stdin with
    | None -> acc
    | Some "" -> acc
    | Some line ->
      (* printf "Line: %s\n" line; *)
      let item = Hashtbl.create 10 in
      let segments = String.sub line 1 (String.length line - 2) |> String.split_on_char ',' in
      (* printf "Segments: %d\n" (List.length segments); *)
      (* printf "item count: %d\n" (Hashtbl.length item); *)
      let rec process_segments segments item =
        match segments with
        | [] -> item
        | segment :: rest ->
          match String.split_on_char '=' segment with
          | [key; value] ->
            Hashtbl.add item key (int_of_string value);
            process_segments rest item
          | _ -> failwith "Invalid segment"
      in
      let item' = process_segments segments item in
      (* printf "item' count: %d\n" (Hashtbl.length item'); *)
      match (accept workshops item' "in") with
      | true -> 
        let total = Hashtbl.fold (fun _ v acc -> acc + v) item' 0 in
        printf "%s -> %d\n" line total;
        process_items' (acc + total)
      | false ->
        (* printf "Rejected: %s\n" line; *)
        process_items' acc
  in
  process_items' 0
;;

let sort_parts =
  let workshops = Hashtbl.create 10 in
  let rec sort_parts' workshops =
    (* read up to first blank line to get the state rules *)
    match In_channel.input_line In_channel.stdin with
    | None -> 0
    | Some "" ->
      printf "Gathered Workshops:\n";
      process_items workshops
    | Some line ->
      (* printf "Line: %s\n" line; *)
      let workshops' = parse_state_rule workshops line in
    
      sort_parts' workshops'
  in
  sort_parts' workshops
;;

let () =
  let part1 =
    match sort_parts with
    | 0 -> failwith "No parts found"
    | score -> score
  in
  printf "19.1: %d\n" part1;

(*
  dune build puzzle_19_1.exe && cat ../data/day-19-test | dune exec ./puzzle_19_1.exe 
*)  