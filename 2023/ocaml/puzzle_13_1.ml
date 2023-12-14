open Core

let get_data = 
  let rec get_data' all_data running_data =
    match In_channel.input_line In_channel.stdin with
    | None -> 
      let grid = Array.of_list (List.rev running_data) in
      all_data @ [grid]
    | Some "" -> 
      let grid = Array.of_list (List.rev running_data) in
      get_data' (all_data @ [grid]) []
    | Some line -> 
      let row = Array.of_list (String.to_list line) in
      get_data' all_data (row :: running_data)
  in
  get_data' [] []
;;

let print_grid grid =
  Array.iter grid ~f:(fun row ->
    Array.iter row ~f:(fun c -> printf "%c" c);
    printf "\n"
  )
;;

(* given a grid and axis (vertical|horizontal)
   find the row or column that divides the grid 
   symmetrically *)
let get_symmetry grid axis =
  match axis with 
  | `vertical -> 
    let cols = Array.length grid.(0) in
    let rec get_symmetry' col =
      if col >= cols then
        None
      else
        let rec check_row row =
          if row >= Array.length grid then
            Some col
          else
            let c1 = grid.(row).(col) in
            let c2 = grid.(row).(cols - col - 1) in
            if (Char.equal c1 c2) then
              check_row (row + 1)
            else
              None
        in
        match check_row 0 with
        | None -> get_symmetry' (col + 1)
        | Some col -> Some col
    in
    get_symmetry' 0
  | `horizontal ->
    let rows = Array.length grid in
    let rec get_symmetry' row =
      if row >= rows then
        None
      else
        let rec check_col col =
          if col >= Array.length grid.(0) then
            Some row
          else
            let c1 = grid.(row).(col) in
            let c2 = grid.(rows - row - 1).(col) in
            if (Char.equal c1 c2) then
              check_col (col + 1)
            else
              None
        in
        match check_col 0 with
        | None -> get_symmetry' (row + 1)
        | Some row -> Some row
    in
    get_symmetry' 0
  (* let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let rec get_symmetry' row col =
    if row >= rows || col >= cols then
      None
    else
      let c1 = grid.(row).(col) in
      let c2 = grid.(row).(cols - col - 1) in
      if (Char.equal c1 c2) then
        get_symmetry' (row + 1) col
      else
        Some (row, col)
  in
  match axis with
  | `vertical -> get_symmetry' 0 (cols / 2)
  | `horizontal -> get_symmetry' (rows / 2) 0 *)
;;

let rec sum_symmetry sum grids =
  match grids with
  | [] -> sum
  | grid :: grids' -> 
    printf "grid(cols, rows): (%d, %d)\n" (Array.length grid.(0)) (Array.length grid);
    let h_sym = get_symmetry grid `horizontal in
    printf "h_sym: %s\n" (Option.value_map h_sym ~default:"None" ~f:(fun x -> Int.to_string x));
    let v_sym = get_symmetry grid `vertical in
    printf "v_sym: %s\n" (Option.value_map v_sym ~default:"None" ~f:(fun x -> Int.to_string x));
    let sum' = sum + 1 in
    sum_symmetry sum' grids'
  ;;

let () =
  (* this should be a list of Arrays [][] *)
  let data = get_data in
  let first_grid = List.hd_exn data in
  print_grid (first_grid);
  printf "\n";

  (* List.iter data ~f:(fun grid -> print_grid grid; printf "\n"); *)

let first_puzzle = 
  match sum_symmetry 0 data with
  | 0 -> "You're Lost\n"
  | combos -> Printf.sprintf "13.1: %d\n" combos 
in
  Printf.printf "%s" first_puzzle
;;
  (*
  dune build puzzle_13_1.exe && cat ../data/day-13-test | dune exec ./puzzle_13_1.exe  
  *)