open Core

module RaceInfo = struct
  type t = {
    time: int;
    distance: int;  }
end

let data = Helper.read_lines "../data/day-06"

let rec populate_race_info data time distance =
  match data with 
  | [] -> 
    printf "Done: \n";
    [RaceInfo.{time = time; distance = distance}]
  | line :: rest when Helper.starts_with line "Time:" -> 
    (* printf "Time: %s\n" line; *)
    let step1 = Helper.trim (String.drop_prefix line 5) in
    printf "Time: %s\n" step1;
    let step2 = (Str.global_replace (Str.regexp " +") "" step1) in
    printf "Step2: %s\n" step2;
    let step3 = int_of_string step2 in
    printf "Time: %d\n" step3;
    populate_race_info rest step3 distance
  | line :: rest when Helper.starts_with line "Distance:" -> 
    (* printf "Distance: %s\n" line; *)
    let step1 = Helper.trim (String.drop_prefix line 9) in
    let step2 = (Str.global_replace (Str.regexp " +") "" step1) in
    let step3 = int_of_string step2 in
    populate_race_info rest time step3
  | _ -> 
    printf "Default thing: \n";
    populate_race_info [] time distance
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
  let race_data = populate_race_info data 0 0 in
  printf "Races: %d\n" (List.length race_data);
  match sum_winning_ways 1 race_data with
  | 0 -> printf "No soup for you\n"
  | ways_to_win -> printf "Day 6.2: %d\n" ways_to_win

  (*
  dune build puzzle_06_2.exe && dune exec ./puzzle_06_2.exe  
  *)