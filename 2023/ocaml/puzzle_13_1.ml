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

let calc_badness grid part2 axis =
  let rows = List.length grid in
  let cols = List.hd_exn grid |> String.length in
  let calc_axis_badness pos delta_pos get_other_pos =
    let badness = ref 0 in
    for delta = 0 to delta_pos - 1 do
      let pos1 = pos - delta in
      let pos2 = pos + 1 + delta in
      if pos1 >= 0 && pos2 < delta_pos then
        for other = 0 to get_other_pos - 1 do
          let char1, char2 = match axis with
            | `Vertical -> List.nth_exn grid other |> fun row -> row.[pos1], row.[pos2]
            | `Horizontal -> grid.(pos1).[other], grid.(pos2).[other]
          in
          if char1 <> char2 then incr badness
        done
    done;
    !badness
  in
  let total = ref 0 in
  for i = 0 to (match axis with `Vertical -> cols - 1 | `Horizontal -> rows - 1) - 1 do
    let badness = match axis with
      | `Vertical -> calc_axis_badness i cols rows
      | `Horizontal -> calc_axis_badness i rows cols
    in
    if badness = (if part2 then 1 else 0) then
      total := !total + (if axis = `Horizontal then 100 else 1) * (i + 1)
  done;
  !total

let rec sum_symmetry sum grids =
  match grids with
  | [] -> sum
  | grid :: grids' -> 
    printf "grid(cols, rows): (%d, %d)\n" (Array.length grid.(0)) (Array.length grid);
    let symmetry = (calc_badness grid false `Vertical) + (calc_badness grid false `Horizontal) in
    printf "symmetry: %d\n" symmetry;
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