open Core

module Range = struct
  type t = {
    start: int;
    finish: int;
    delta: int;
  }
end

let maps = Hashtbl.create (module String)

let display_hash_map hash_map = 
  Hashtbl.iteri hash_map ~f:(fun ~key ~data ->
    printf "%s:\n" key;
    List.iter data ~f:(fun range ->
      printf "\t%d %d %d\n" range.Range.start range.Range.finish range.Range.delta
    );
    printf "\n"
  )

let lookup hash_map key source = 
  match Hashtbl.find hash_map key with
  | Some range_list -> 
    let rec find_in_ranges ranges = 
      match ranges with
      | [] -> source
      | range :: rest -> 
        if (source >= range.Range.start && source <= range.Range.finish) then
          (source + range.Range.delta)
        else
          find_in_ranges rest 
    in
    find_in_ranges range_list
  | None -> source
  ;;


let update_hash_map hash_map key range = 
  match Hashtbl.find hash_map key with
  | Some value -> 
    let new_value = range :: value in
    let _ = Hashtbl.remove hash_map key in
    let _ = Hashtbl.add hash_map ~key:key ~data:new_value in
    ()
  | None ->
    let ranges = [range] in
    let _ = Hashtbl.add hash_map ~key:key ~data:ranges in
    ()
;;

let gather_seeds input =
  (* Strip the label 'seeds: ' from the input *) 
  let seeds = String.drop_prefix input 7 in
  (* Convert the remainder of the line to a list of int *)
  let seed_list = Helper.string_to_int_list seeds in
  seed_list
;;

let calculate_min_location seed_list maps = 
  let rec find_min_location seed_list min_location = 
    match seed_list with
    | [] -> min_location
    | seed :: rest -> 
      let soil = lookup maps "seed-to-soil" seed in
      let fertilizer = lookup maps "soil-to-fertilizer" soil in
      let water = lookup maps "fertilizer-to-water" fertilizer in
      let light = lookup maps "water-to-light" water in
      let temperature = lookup maps "light-to-temperature" light in
      let humidity = lookup maps "temperature-to-humidity" temperature in
      let location = lookup maps "humidity-to-location" humidity in
      
      let min_location' = if (location < min_location) then location else min_location in
      find_min_location rest min_location'
  in
  find_min_location seed_list 9999999999

let rec find_lowest_location min_location seed_list task maps  = 
  match In_channel.input_line In_channel.stdin with
  (* ignore blank lines *)
  | Some "" -> 
    find_lowest_location min_location seed_list task maps
  (* Gather seeds *)
  | Some line when (Helper.starts_with line "seeds:") -> 
    printf "Gather %s\n" line;
    let seeds = gather_seeds line in
    find_lowest_location min_location seeds task maps
  (* switch to task *)
  | Some line when (String.contains line ':') -> 
    (* printf "Determine Task: %s\n" line; *)
    let regex = (Str.regexp_string " map:") in
    let new_task = (Str.global_replace regex "" line) in

    find_lowest_location min_location seed_list new_task maps
  (* process map *)
  | Some line -> 
    (* printf "Task is: %s\t%s \n" line task; *)

    let input = Helper.string_to_int_list line in
    (* printf "..Input: %d %d %d\n" (List.nth_exn input 0) (List.nth_exn input 1) (List.nth_exn input 2); *)
    let _ = match input with
      | [dest; src; len] -> 
        let range = Range.{start = src; finish = src + len - 1; delta = dest - src} in
        update_hash_map maps task range
      | _ -> ()
    in

    find_lowest_location min_location seed_list task maps
  (* End of input *)
  | None -> 
    (* Now we do the work. *)
    display_hash_map maps;
    let min_location' = calculate_min_location seed_list maps in
    min_location'

let () = 
  (* let r = Range.{start = 0; finish = 0; delta = 0} in
  printf "Range: %d %d %d\n" r.start r.finish r.delta; *)
  match find_lowest_location 9999999999 [] "" maps with
  | 0 -> printf "No soup for you\n"
  | calibrations -> printf "Day 5.1: %d\n" calibrations

  (*
  dune build puzzle_05_2.exe && cat ../data/day-05-test | dune exec ./puzzle_05_2.exe  
  *)