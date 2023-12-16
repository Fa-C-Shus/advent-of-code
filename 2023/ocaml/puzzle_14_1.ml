open Core

module IntTriple = struct
  type t = int * int * int
  let compare (a1, b1, c1) (a2, b2, c2) =
    match Int.compare a1 a2 with
    | 0 -> (match Int.compare b1 b2 with
            | 0 -> Int.compare c1 c2
            | cmp -> cmp)
    | cmp -> cmp

  let t_of_sexp sexp = 
    (* Define conversion from S-expression to t *)
    (* Placeholder implementation *)
    let open Core.Sexp in
    match sexp with
    | List [Atom a; Atom b; Atom c] -> (int_of_string a, int_of_string b, int_of_string c)
    | _ -> failwith "Invalid S-expression for IntTriple"

  let sexp_of_t (a, b, c) = 
    (* Define conversion from t to S-expression *)
    Core.Sexp.List [Core.Sexp.Atom (string_of_int a); Core.Sexp.Atom (string_of_int b); Core.Sexp.Atom (string_of_int c)]
end

module IntTripleSet = Set.Make(IntTriple)

module IntTripleMap = Map.Make(IntTriple)

(* This type alias is just for convenience *)
(* type trial_map = int IntTripleMap.t *)

let filename = "../data/day-14";;

let grid = In_channel.read_lines filename
  |> List.map ~f:(fun line -> String.to_list line)
  |> Array.of_list_map ~f:Array.of_list
;;

let print_grid grid =
  Array.iter grid ~f:(fun row ->
    Array.iter row ~f:(fun c -> printf "%c" c);
    printf "\n"
  );
  printf "\n"
;;

let score grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let ans = ref 0 in

  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      if Char.equal grid.(r).(c) 'O' then
        ans := !ans + (rows - r)
    done
  done;
  !ans
;;

let rotate_grid grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in

  (* Create a new grid with transposed dimensions *)
  let new_grid = Array.init cols ~f:(fun _ -> Array.create ~len:rows '?') in

  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      new_grid.(c).(rows - 1 - r) <- grid.(r).(c)
    done
  done;

  new_grid
;;

let tilt_grid grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in

  (* Function to roll 'O's up in a single column *)
  let roll_column col =
    for _ = 0 to rows - 1 do
      for row = 1 to rows - 1 do  (* Start from the second row *)
        match grid.(row).(col), grid.(row - 1).(col) with
        | 'O', '.' -> 
          grid.(row).(col) <- '.';
          grid.(row - 1).(col) <- 'O';
        | _ -> ()
      done
    done
  in

  (* Apply the column-wise rolling to the entire grid *)
  for col = 0 to cols - 1 do
    roll_column col
  done;
  grid
;;

let spin_grid grid =
  let rec spin grid times =
    (* printf "Spinning %d times\n" times; *)
  match times with
  | 0 -> grid
  | _ -> 
    let rolled_grid = tilt_grid grid in
    (* print_grid rolled_grid; *)
    let rotated_grid = rotate_grid rolled_grid in
    (* print_grid rotated_grid; *)
    spin rotated_grid (times - 1)
  in
  spin grid 4
;;

let part_1 grid =
  let grid' = tilt_grid grid in
  print_grid grid';
  score grid'
;;

let rec generate_cycle_list grid trial index cycle_length cycle_start cycle_list =
  let limit = 1000000000 in
  match (index < cycle_length) with
  | true -> 
    let grid' = spin_grid grid in
    let score' = score grid' in
    (* add this score to the end of cycle_list *)
    printf "Trial: %d\tCycle Idx: %d\tScore: %d\n" trial index score';
    let cycle_list' = score' :: cycle_list in
    generate_cycle_list grid' (trial + 1) (index + 1) cycle_length cycle_start cycle_list'
  | false -> 
    printf "what will the score be after %n spins?\n" limit;
    printf "Cycle length: %d\n" cycle_length;
    (* we are currently on the Nth trial where N = trial
        we need to calculate where in the cycle we would
        be at trials = limit 
        the remaining trials will loop through the cycle
        until we reach the limit. 
       *)
    let cycle_offset = (limit - cycle_start) mod cycle_length in
    let cycle_index = cycle_start + cycle_offset in
    printf "Cycle start: %d\n" cycle_start;
    printf "Cycle offset: %d\n" cycle_offset;
    printf "Cycle index: %d\n" cycle_index;
    printf "Cycle score: %d\n" (List.nth_exn cycle_list cycle_offset);
    (List.nth_exn cycle_list cycle_offset)
;;

let part_2 grid limit =
  printf "what will the score be after %n spins?\n" limit;
  let rec cycle grid trial last_three observations trial_map =
    match last_three with
    | [a; b; c] ->
      let key = (a, b, c) in
      (match Map.find trial_map key with
      | Some previous_trial ->
        printf "Cycle detected at trial %d, cycle started at trial %d\n" trial previous_trial;
        printf "\tLast three scores: %d, %d, %d\n" a b c;
        (* Now that we know the limit, 
           the cycle start 'previous_trial' 
           and the cycle length 'trial' - 'previous_trial'
           we need to calculate where in the cycle we would 
           be at trials = limit *)
        (* let cycle_length = trial - previous_trial in
        let cycle_start = previous_trial in
        let cycle_offset = (limit - cycle_start) mod cycle_length in
        let cycle_index = cycle_start + cycle_offset in
        printf "\tCycle length: %d\n" cycle_length;
        printf "\tCycle start: %d\n" cycle_start;
        printf "\tCycle offset: %d\n" cycle_offset;
        printf "\tCycle index: %d\n" cycle_index;
        let cycle_score = 
          match Map.find trial_map (key_at_index trial_map 103) with
          | Some score -> score
          | None -> 0
        in
        (* I want to know the key of this found map element *)
        printf "\tCycle key: %s\n" (IntTriple.sexp_of_t (key_at_index trial_map 118) |> Sexp.to_string);
        printf "\tCycle score: %d\n" cycle_score;
        printf "Map Length: %d\n" (Map.length trial_map);
        printf "Enumerate Map:\n";
        Map.iteri trial_map ~f:(fun ~key ~data -> printf "\t%s: %d\n" (IntTriple.sexp_of_t key |> Sexp.to_string) data); *)
        generate_cycle_list grid trial 0 (trial - previous_trial) previous_trial []
        (* observations *)
      | None ->
        let grid' = spin_grid grid in
        let score' = score grid' in
        let trial_map' = 
          match Map.add trial_map ~key:key ~data:trial with
          | `Ok new_map -> new_map
          | `Duplicate -> trial_map (* This case should not happen as we're checking with Map.find above *)
        in
        cycle grid' (trial + 1) [b; c; score'] (Set.add observations key) trial_map')
    | _ ->
      let grid' = spin_grid grid in
      let score' = score grid' in
      cycle grid' (trial + 1) [0; 0; score'] observations trial_map
  in
  cycle grid 0 [] IntTripleSet.empty IntTripleMap.empty
;;

let () = 
  match (part_1 grid) with
  | 0 -> printf "You're Lost\n"
  | score -> printf "14.1: %d\n" score;;

  let grid = In_channel.read_lines filename
  |> List.map ~f:(fun line -> String.to_list line)
  |> Array.of_list_map ~f:Array.of_list in 
  (* print_grid grid;   *)

  match (part_2 grid 1000000000) with
  | 0 -> printf "You're Lost\n"
  | score -> printf "14.2: %d\n" score;;

  (*
  dune build puzzle_14_1.exe && dune exec ./puzzle_14_1.exe   
  *)