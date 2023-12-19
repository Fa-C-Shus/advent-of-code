[@@@warning "-32"]
[@@@warning "-27"]
[@@@warning "-26"]
[@@@warning "-39"]

open Printf 

let create_dict str size =
  let dict = Hashtbl.create (String.length str) in
  String.iter (fun key ->
    Hashtbl.add dict (String.make 1 key) (1, size)
  ) str;
  dict
;;

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

let count_acceptable workshops =
  let dict = create_dict "xmas" 4000 in

  let rec count_acceptable' workshops xmas_dict name =
    (* printf "%s {" name;
    Hashtbl.iter (fun key (lo, hi) -> printf "'%s': (%d-%d), " key lo hi) xmas_dict;
    printf "}\n"; *)
    match name with
    | "R" -> 0
    | "A" -> 
      let total = Hashtbl.fold (fun _ (lo, hi) acc -> acc + (acc * (hi - lo))) xmas_dict 1 in
      (* printf "Total: %d\n" total; *)
      total
    | _ -> 
      let (rules, fallback) = Hashtbl.find workshops name in

      (* printf "Rules to process: %d\n" (List.length rules); *)

      let rec process_rules rules xmas_dict break total =
        match rules with
        | [] when not break -> 
          printf "No rules were applied %d\n" total;
          total + (count_acceptable' workshops xmas_dict fallback)
        | _ when break -> 
          printf "break -> %d\n" total;
          total
        | [] ->
          printf "Empty NOT break\n";
          total
          (* we have rules to assess *)
        | (key, cmp, value, target) :: rest ->
          (* printf "Processing rule: %s %s %d -> %s\n" key cmp value target; *)
          let (lo, hi) = Hashtbl.find xmas_dict key in
          let (good, bad) =
            match cmp with
            | "<" -> 
              (* printf "< "; *)
              ((lo, min (value - 1) hi), (max value lo, hi))
            | ">" ->
              (* printf "> "; *)
              ((max (value + 1) lo, hi), (lo, min value hi))
            | _ -> failwith "Invalid comparison"
          in
          (* printf "%s -> (%d, %d), (%d, %d)\n" key (fst good) (snd good) (fst bad) (snd bad);
          printf "Total: %d\n" total; *)
          let (total', good_dict) =
            match (fst good) <= (snd good) with
            | true ->
              let xmas_dict_good = Hashtbl.copy xmas_dict in
              Hashtbl.replace xmas_dict_good key good;
              (total + (count_acceptable' workshops xmas_dict_good target), xmas_dict_good)
            | false -> (total, xmas_dict)
          in

          let (total'', next_dict, break') =
            match (fst bad) <= (snd bad) with
            | true ->
              let xmas_dict_bad = Hashtbl.copy xmas_dict in
              Hashtbl.replace xmas_dict_bad key bad;
              (total', xmas_dict_bad, false)
            | false -> (total', xmas_dict, true)
          in
          process_rules rest next_dict break' total''
      in
      process_rules rules xmas_dict false 0 
    in
  count_acceptable' workshops dict "in"
;;

let sort_parts =
  let workshops = Hashtbl.create 10 in
  let rec sort_parts' workshops =
    (* read up to first blank line to get the state rules *)
    match In_channel.input_line In_channel.stdin with
    | None -> 0
    | Some "" ->
      printf "Gathered Workshops:\n";
      count_acceptable workshops
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
  printf "19.2: %d\n" part1;

(*
  dune build puzzle_19_2.exe && cat ../data/day-19-test | dune exec ./puzzle_19_2.exe 
   93216
  187296
*)  