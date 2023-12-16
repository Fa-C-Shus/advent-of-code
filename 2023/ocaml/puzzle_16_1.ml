open Core

module ParticleKey = struct
  type t = int * int * char
  let compare (a1, b1, c1) (a2, b2, c2) =
    match Int.compare a1 a2 with
    | 0 -> (match Int.compare b1 b2 with
            | 0 -> Char.compare c1 c2
            | cmp -> cmp)
    | cmp -> cmp

  let t_of_sexp sexp = 
    let open Core.Sexp in
    match sexp with
    | List [Atom a; Atom b; Atom c] -> (int_of_string a, int_of_string b, Char.of_string c)
    | _ -> failwith "Invalid S-expression for ParticleKey"

  let sexp_of_t (a, b, c) = 
    Core.Sexp.List [Core.Sexp.Atom (string_of_int a); Core.Sexp.Atom (string_of_int b); Core.Sexp.Atom (Char.to_string c)]
end

module ParticleKeySet = Set.Make(ParticleKey)

module ParticleKeyMap = Map.Make(ParticleKey)

module Coord = struct
  type t = int * int
  let compare (x0, y0) (x1, y1) =
    match compare x0 x1 with
    | 0 -> compare y0 y1
    | c -> c

  let t_of_sexp sexp =
    let open Core.Sexp in
    match sexp with
    | List [x; y] -> (Core.Int.t_of_sexp x, Core.Int.t_of_sexp y)
    | _ -> failwith "Coord.t_of_sexp: list of two integers expected"

  let sexp_of_t (x, y) =
    Core.Sexp.List [Core.Int.sexp_of_t x; Core.Int.sexp_of_t y]
end

module CoordSet = Core.Set.Make(Coord)

let grid = In_channel.read_lines "../data/day-16"
  |> List.map ~f:(fun line -> String.to_list line)
  |> Array.of_list_map ~f:Array.of_list
  ;;

(* let print_grid grid =
  Array.iter grid ~f:(fun row ->
    Array.iter row ~f:(fun c -> printf "%c" c);
    printf "\n"
  )
;; *)

(* let print_energized_grid grid energized =
  Array.iteri grid ~f:(fun row_index row ->
    Array.iteri row ~f:(fun col_index _ ->
      let key = (row_index, col_index) in
      if Set.mem energized key then
        printf "#"
      else
        printf "." 
    );
    printf "\n"
  )
;; *)

let follow_the_light grid row col dir =
  let visited = ref ParticleKeySet.empty in
  let visited_coords = ref CoordSet.empty in
  let width = Array.length grid.(0) in
  let height = Array.length grid in
  let rec follow_the_light' row col dir =
    let key = (row, col, dir) in
    match (row, col, dir) with
    (* out of bounds *)
    | (r, c, _) when r < 0 || c < 0 || r >= height || c >= width -> 
      (* printf "Out of bounds\n"; *)
      !visited_coords
    (* I have been on this cell AND moving in this direction; I'm in a loop *)
    | (_, _, _) when Set.mem !visited key ->
      (* printf "Loop\n"; *)
      !visited_coords
    | _ ->
      match grid.(row).(col) with
      | '.' ->
        (* printf "Empty\n"; *)
        visited := Set.add !visited key;
        visited_coords := Set.add !visited_coords (row, col);
        (match dir with
        | 'r' -> follow_the_light' row (col + 1) dir
        | 'l' -> follow_the_light' row (col - 1) dir  
        | 'u' -> follow_the_light' (row - 1) col dir
        | 'd' -> follow_the_light' (row + 1) col dir
        | _ -> failwith "Invalid direction")
      | '|' ->
        (* printf "Vertical Splitter \n"; *)
        visited := Set.add !visited key;
        visited_coords := Set.add !visited_coords (row, col);
        (match dir with
        | 'r' | 'l' -> 
          follow_the_light' (row + 1) col 'd'
          |> Set.union (follow_the_light' (row - 1) col 'u')
        | 'u' -> follow_the_light' (row - 1) col dir
        | 'd' -> follow_the_light' (row + 1) col dir
        | _ -> failwith "Invalid direction")
      | '-' ->
        (* printf "Horizontal Splitter \n"; *)
        visited := Set.add !visited key;
        visited_coords := Set.add !visited_coords (row, col);
        (match dir with
        | 'r' -> follow_the_light' row (col + 1) dir
        | 'l' -> follow_the_light' row (col - 1) dir  
        | 'u' | 'd' -> 
          follow_the_light' row (col + 1) 'r'
          |> Set.union (follow_the_light' row (col - 1) 'l')
        | _ -> failwith "Invalid direction")
      | '/' ->
        (* printf "Slash Turn \n"; *)
        visited := Set.add !visited key;
        visited_coords := Set.add !visited_coords (row, col);
        (match dir with
        | 'r' -> follow_the_light' (row - 1) col 'u'
        | 'l' -> follow_the_light' (row + 1) col 'd'
        | 'u' -> follow_the_light' row (col + 1) 'r'
        | 'd' -> follow_the_light' row (col - 1) 'l'
        | _ -> failwith "Invalid direction")
      | '\\' ->
        (* printf "Backslash Turn \n"; *)
        visited := Set.add !visited key;
        visited_coords := Set.add !visited_coords (row, col);
        (match dir with
        | 'r' -> follow_the_light' (row + 1) col 'd'
        | 'l' -> follow_the_light' (row - 1) col 'u'
        | 'u' -> follow_the_light' row (col - 1) 'l'
        | 'd' -> follow_the_light' row (col + 1) 'r'
        | _ -> failwith "Invalid direction")
      | _ ->
        (* printf "row: %d, col: %d, dir: %c, on: %c\n" row col dir c; *)
        visited := Set.add !visited key;
        visited_coords := Set.add !visited_coords (row, col);
        follow_the_light' row col dir
  in
  let been_there = follow_the_light' row col dir in 
  (* print_energized_grid grid !visited_coords; *)
  Set.length been_there
;;

let create_edge_directions grid =
  let edges = ref [] in
  let m = Array.length grid in
  let n = Array.length grid.(0) in

  (* Top edge *)
  for col = 0 to n - 1 do
    edges := (0, col, 'd') :: !edges
  done;

  (* Bottom edge *)
  for col = 0 to n - 1 do
    edges := (m - 1, col, 'u') :: !edges
  done;

  (* Left edge, excluding corners *)
  for row = 1 to m - 2 do
    edges := (row, 0, 'r') :: !edges
  done;

  (* Right edge, excluding corners *)
  for row = 1 to m - 2 do
    edges := (row, n - 1, 'l') :: !edges
  done;

  !edges
;; 

let solve_puzzle grid part =
  (* let height = Array.length grid in
  let width = Array.length grid.(0) in *)
  let starts = 
    match part with
    | 1 -> [(0, 0, 'r')]
    (* Make this a list of all the cells outside 
       the grid heading into the grid *)
    | 2 -> create_edge_directions grid
    | _ -> failwith "Invalid part"
  in
  let rec solve_puzzle' max grid starts =
    match starts with
    | [] -> 
      printf "No more particles\n";
      max
    | (row, col, dir) :: rest ->
      printf "row: %d, col: %d, dir: %c\n" row col dir;
      let energized = follow_the_light grid row col dir in
      let max' = 
        match (energized > max) with
        | true -> energized
        | false -> max
       in
      solve_puzzle' max' grid rest
  in
  solve_puzzle' 0 grid starts
  ;;

let () =
  (* print_grid grid; *)
  match solve_puzzle grid 1 with
  | 0 -> printf "Whiskey Tango Firetruck\n"
  | energized -> printf "16.1: %d\n" energized;

  match solve_puzzle grid 2 with
  | 0 -> printf "Whiskey Tango Firetruck\n"
  | energized -> printf "16.2: %d\n" energized;
;;

(*
  dune build puzzle_16_1.exe && dune exec ./puzzle_16_1.exe   
*)