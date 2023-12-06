open Core

module RaceInfo = struct
  type t = {
    time: int;
    distance: int;  }
end

let data = Helper.read_lines "../data/day-06"

let rec combine_list times distances results =
  match (times, distances) with
  | ([], []) -> results
  | (t :: t_rest, d :: d_rest) -> 
    let new_item = {RaceInfo.time = t; distance = d} in
    combine_list t_rest d_rest (new_item :: results)
  | _ -> results
  ;;

let rec populate_race_info data times distances =
  match data with 
  | [] -> 
    printf "Done: \n";
    combine_list times distances []
  | line :: rest when Helper.starts_with line "Time:" -> 
    (* printf "Time: %s\n" line; *)
    let time_list_string = Helper.trim (String.drop_prefix line 5) in
    let time_list = Helper.string_to_int_list time_list_string in
    printf "Time: %s\n" time_list_string;
    populate_race_info rest time_list distances
  | line :: rest when Helper.starts_with line "Distance:" -> 
    (* printf "Distance: %s\n" line; *)
    let dist_list_string = Helper.trim (String.drop_prefix line 9) in
    let dist_list = Helper.string_to_int_list dist_list_string in
    (* printf "Distance: %s\n" dist_list_string; *)
    populate_race_info rest times dist_list
  | _ -> 
    printf "Default thing: \n";
    populate_race_info [] times distances
  ;;
  
  let rec find_wins accum running_time max_time curr_record =
    (* time becomes speed * remaining time *)
    let distance = running_time * (max_time - running_time) in
    match running_time with
    | _ when running_time = max_time -> accum
    | _ when distance > curr_record.RaceInfo.distance -> 
      find_wins (accum + 1) (running_time + 1) max_time curr_record
    | _ ->
      find_wins accum (running_time + 1) max_time curr_record

    ;;

  let rec sum_winning_ways accum data = 
    match data with
    | [] -> accum
    | record :: tl -> 
      let wins = find_wins 0 1 record.RaceInfo.time record in
      printf "Data: %d in %d\n" record.RaceInfo.distance record.RaceInfo.time;
      sum_winning_ways (accum * wins) tl
    ;;
    
let () = 
  let race_data = populate_race_info data [] [] in
  printf "Races: %d\n" (List.length race_data);
  match sum_winning_ways 1 race_data with
  | 0 -> printf "No soup for you\n"
  | ways_to_win -> printf "Day 6.1: %d\n" ways_to_win

  (*
  dune build puzzle_06_1.exe && dune exec ./puzzle_06_1.exe  
  *)