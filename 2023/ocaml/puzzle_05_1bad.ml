open Core

module StringSet = Set.Make(String)
;;

let gather_seeds input =
  (* Strip the label 'seeds: ' from the input *) 
  let seeds = String.drop_prefix input 7 in
  (* Convert the remainder of the line to a list of int *)
  let seed_list = Helper.string_to_int_list seeds in
  seed_list
;;

let update_hash_map hash_map dest src len = 
  let rec update_hash_map' hash_map dest src len = 
    match len with
    | 0 -> 
      printf "Hash Map: %d\n" (Hashtbl.length hash_map);
      hash_map
    | _ -> 
      if len mod 100000 = 0 then
        Printf.printf "Len: %d\n" len;
      match Hashtbl.find hash_map src with
      | Some _ -> 
        let _ = Hashtbl.remove hash_map src in
        let _ = Hashtbl.add hash_map ~key:src ~data:dest in
        update_hash_map' hash_map (dest + 1) (src + 1) (len - 1)
      | None ->
        let _ = Hashtbl.add hash_map ~key:src ~data:dest in
        update_hash_map' hash_map (dest + 1) (src + 1) (len - 1)
  in
  update_hash_map' hash_map dest src len
;;

let find_with_default hash_table key default_value =
  match Hashtbl.find hash_table key with
  | Some value -> value
  | None -> default_value
;;

let rec find_min_location min_location seed_list seed2soil soil2fertilizer fertilizer2water water2light light2temperature temperature2humidity humidity2location = 
  match seed_list with
  | [] -> min_location
  | seed :: seed_list' -> 
    let soil = find_with_default seed2soil seed seed in
    let fertilizer = find_with_default soil2fertilizer soil soil in
    let water = find_with_default fertilizer2water fertilizer fertilizer in
    let light = find_with_default water2light water water in
    let temperature = find_with_default light2temperature light light in
    let humidity = find_with_default temperature2humidity temperature temperature in
    let location = find_with_default humidity2location humidity humidity in
    let min_location' = if location < min_location then location else min_location in
    printf "Current location: %d\n" min_location';
    find_min_location min_location' seed_list' seed2soil soil2fertilizer fertilizer2water water2light light2temperature temperature2humidity humidity2location

let rec sum_prizes min_location seed_list task seed2soil soil2fertilizer fertilizer2water water2light light2temperature temperature2humidity humidity2location = 
  match In_channel.input_line In_channel.stdin with
  (* ignore blank lines *)
  | Some "" -> 
      sum_prizes min_location seed_list task seed2soil soil2fertilizer fertilizer2water water2light light2temperature temperature2humidity humidity2location
  (* Gather seeds *)
  | Some line when (Helper.starts_with line "seeds:") -> 
    (* printf "Gather Seeds: %s\n" line; *)
    let seeds = gather_seeds line in
    sum_prizes min_location seeds task seed2soil soil2fertilizer fertilizer2water water2light light2temperature temperature2humidity humidity2location
  (* switch to task *)
  | Some line when (String.contains line ':') -> 
    (* printf "Determine Task: %s\n" line; *)
    let regex = (Str.regexp_string " map:") in
    let new_task = (Str.global_replace regex "" line) in

    sum_prizes min_location seed_list new_task seed2soil soil2fertilizer fertilizer2water water2light light2temperature temperature2humidity humidity2location
  (* process map *)
  | Some line -> 
    (* printf "Task is: %s\t%s \n" line task; *)
    let input = Helper.string_to_int_list line in
    let _ =
      match (task, input) with
      | ("seed-to-soil", [dest; src; len]) -> 
        let _ = update_hash_map seed2soil dest src len in
        ()
      | ("soil-to-fertilizer", [dest; src; len]) -> 
        let _ = update_hash_map soil2fertilizer dest src len in
        ()
      | ("fertilizer-to-water", [dest; src; len]) -> 
        let _ = update_hash_map fertilizer2water dest src len in
        ()
      | ("water-to-light", [dest; src; len]) -> 
        let _ = update_hash_map water2light dest src len in
        ()
      | ("light-to-temperature", [dest; src; len]) -> 
        let _ = update_hash_map light2temperature dest src len in
        ()
      | ("temperature-to-humidity", [dest; src; len]) -> 
        let _ = update_hash_map temperature2humidity dest src len in
        ()
      | ("humidity-to-location", [dest; src; len]) -> 
        let _ = update_hash_map humidity2location dest src len in
        ()
      | _ -> () in

    sum_prizes min_location seed_list task seed2soil soil2fertilizer fertilizer2water water2light light2temperature temperature2humidity humidity2location
(* End of input *)
  | None -> 
    (* Now we do the work. *)
    let min_location' = 
      find_min_location min_location seed_list seed2soil soil2fertilizer fertilizer2water water2light light2temperature temperature2humidity humidity2location in
    min_location'

let () = 
  let seed2soil = Hashtbl.create (module Int) in
  let soil2fertilizer = Hashtbl.create (module Int) in
  let fertilizer2water = Hashtbl.create (module Int) in
  let water2light = Hashtbl.create (module Int) in
  let light2temperature = Hashtbl.create (module Int) in
  let temperature2humidity = Hashtbl.create (module Int) in
  let humidity2location = Hashtbl.create (module Int) in
  match sum_prizes 9999999999 [] "" seed2soil soil2fertilizer fertilizer2water water2light light2temperature temperature2humidity humidity2location with
  | 0 -> printf "No soup for you\n"
  | calibrations -> printf "Day 5.1: %d\n" calibrations


  (*
  dune build puzzle_05_1bad.exe && cat ../data/day-05-test | dune exec ./puzzle_05_1bad.exe  
  *)