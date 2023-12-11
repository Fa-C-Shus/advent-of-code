open Core

let grid = In_channel.read_lines "../data/day-10"
  |> List.map ~f:(fun line -> String.to_list line)
  |> Array.of_list_map ~f:Array.of_list
  ;;

module CoordBag = Set.Make (struct
  type t = int * int
  let compare = Stdlib.compare

  let sexp_of_t (x, y) = 
    Sexp.List [Int.sexp_of_t x; Int.sexp_of_t y]

  let t_of_sexp sexp = 
    match sexp with
    | Sexp.List [x; y] -> (Int.t_of_sexp x, Int.t_of_sexp y)
    | _ -> failwith "CoordBag.t_of_sexp: list of two integers expected"
end)

let print_grid grid =
  Array.iter grid ~f:(fun row ->
    Array.iter row ~f:(fun c -> printf "%c" c);
    printf "\n"
  )
  ;;

let map_path grid path =
  (* return a new grid the is the orginal grid but shows the 
    original element if it is in the path (CoordBag); 
    otherwise converts the element to a space *)
  Array.mapi grid ~f:(fun y row ->
    Array.mapi row ~f:(fun x c -> 
      if Set.mem path (x, y) then c 
      else ' '
    )
  )

let expand_symbol = function
  | '|' -> [| [|'.'; 'x'; '.'|]; [|'.'; 'x'; '.'|]; [|'.'; 'x'; '.'|] |]
  | 'S' -> [| [|'.'; 'x'; '.'|]; [|'.'; 'x'; '.'|]; [|'.'; 'x'; '.'|] |]
  | '-' -> [| [|'.'; '.'; '.'|]; [|'x'; 'x'; 'x'|]; [|'.'; '.'; '.'|] |]
  | 'L' -> [| [|'.'; 'x'; '.'|]; [|'.'; 'x'; 'x'|]; [|'.'; '.'; '.'|] |]
  | 'J' -> [| [|'.'; 'x'; '.'|]; [|'x'; 'x'; '.'|]; [|'.'; '.'; '.'|] |]
  | '7' -> [| [|'.'; '.'; '.'|]; [|'x'; 'x'; '.'|]; [|'.'; 'x'; '.'|] |]
  | 'F' -> [| [|'.'; '.'; '.'|]; [|'.'; 'x'; 'x'|]; [|'.'; 'x'; '.'|] |]
  (* | 'S' -> [| [|'.'; '.'; '.'|]; [|'.'; 'x'; 'x'|]; [|'.'; 'x'; '.'|] |] *)
  | '.' -> [| [|'.'; '.'; '.'|]; [|'.'; '.'; '.'|]; [|'.'; '.'; '.'|] |]
  | ' ' -> [| [|'.'; '.'; '.'|]; [|'.'; '.'; '.'|]; [|'.'; '.'; '.'|] |]
  | _ -> failwith "Invalid symbol\n"
  ;;

let expand_map grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  Array.init (height * 3) ~f:(fun y ->
    Array.init (width * 3) ~f:(fun x ->
      let symbol = grid.(y / 3).(x / 3) in
      (* printf "symbol: %c\n" symbol; *)
      let expanded_symbol = expand_symbol symbol in
      expanded_symbol.(y mod 3).(x mod 3)
    )
  )
  
let find_start grid =
  (* iterate over the grid until you find the character S *)
  let rec find_start' grid x y =
    match grid.(y).(x) with
    | 'S' -> (x, y)
    | _ -> 
      if x = Array.length grid.(y) - 1 then find_start' grid 0 (y + 1)
      else find_start' grid (x + 1) y
    in
  find_start' grid 0 0
;;

(* Just return the first valid prior ???
   Do I can about what I am? *)
let get_shadow_start grid x y =
  (* let width = (Array.length grid.(0) -1) in
  let height = (Array.length grid -1) in
  let left = 
    match x with
    | 0 -> None (* invalid pipe *)
    | _ -> grid.(y).(x - 1) in
  let right =
    match width = x with
    | true -> None (* invalid pipe *)
    | false -> grid.(y).(x + 1) in
  let up =
    match y with
    | 0 -> None (* invalid pipe *)
    | _ -> grid.(y - 1).(x) in
  let down =
    match height = y with
    | true -> None (* invalid pipe *)
    | false -> grid.(y + 1).(x) in *)
  let width = Array.length grid.(0) in
  let height = Array.length grid in
  let left = if x > 0 then Some grid.(y).(x - 1) else None in
  let right = if x < width - 1 then Some grid.(y).(x + 1) else None in
  let up = if y > 0 then Some grid.(y - 1).(x) else None in
  let down = if y < height - 1 then Some grid.(y + 1).(x) else None in
  let left' = 
    match left with
    | Some '-' -> Some '-'
    | Some 'F' -> Some 'F'
    | Some 'L' -> Some 'L'
    | _ -> None
  in
  let right' = 
    match right with
    | Some '-' -> Some '-'
    | Some 'J' -> Some 'J'
    | Some '7' -> Some '7'
    | _ -> None
  in
  let up' = 
    match up with
    | Some '|' -> Some '|'
    | Some 'F' -> Some 'F'
    | Some '7' -> Some '7'
    | _ -> None
  in
  let down' = 
    match down with
    | Some '|' -> Some '|'
    | Some 'J' -> Some 'J'
    | Some 'L' -> Some 'L'
    | _ -> None
  in
  (* printf "left: %c, right: %c, up: %c, down: %c\n" 
    (Option.value left' ~default:'x')
    (Option.value right' ~default:'x')
    (Option.value up' ~default:'x')
    (Option.value down' ~default:'x'); *)
  match (left', right', up', down') with
  | (Some _, Some _, None, None) -> ('-', x + 1, y)
  | (Some _, None, Some _, None) -> ('J', x, y - 1)
  | (Some _, None, None, Some _) -> ('7', x - 1 , y)
  | (None, Some _, Some _, None) -> ('L', x, y - 1)
  | (None, Some _, None, Some _) -> ('F', x + 1, y)
  | (None, None, Some _, Some _) -> ('|', x, y - 1)
  | _ -> failwith "shit ain'y right"
  ;;  
  
(* 
| is a vertical pipe connecting north and south.
- is a horizontal pipe connecting east and west.
L is a 90-degree bend connecting north and east.
J is a 90-degree bend connecting north and west.
7 is a 90-degree bend connecting south and west.
F is a 90-degree bend connecting south and east.
. is ground; there is no pipe in this tile.
S is the starting position of the animal; there is a pipe on this
*)
let rec navigate_pipe acc grid x y prev_c x1 y1 path = 
  let c = grid.(y).(x) in
  (* printf "char: %c\n" c; *)
  path := Set.add !path (x, y);

  (* printf "path length: %d\n" (Set.length !path); *)

  let dx = x - x1 in
  let dy = y - y1 in 
  let from_dir = 
    match (dx, dy) with 
    | (0, 0) -> "start"
    | (1, 0) -> "left"
    | (-1, 0) -> "right"
    | (0, 1) -> "above"
    | (0, -1) -> "below"
    | (_, _) -> "WTF"
  in
  (* printf "from_dir %s\n" from_dir; *)
  match (c, prev_c) with
  | ('S', '^') -> 
    (* printf "Found start; determine what I should have been\n";
    printf "grid size: %d, %d\n" (Array.length grid.(0)) (Array.length grid); *)
    let (shadow_start, next_x, next_y) = (get_shadow_start grid x y) in
    (* printf "shadow_start: %c, %d, %d\n" shadow_start next_x next_y; *)
    navigate_pipe (acc + 1) grid next_x next_y shadow_start x1 y1 path
  | ('S', _) -> 
    (* printf "Found end\n"; *)
    path
  | ('J', _) ->
    (match from_dir with
    | "left" -> navigate_pipe (acc + 1) grid x (y - 1) c x y path
    | "above" -> navigate_pipe (acc + 1) grid (x - 1) y c x y path
    | _ -> failwith "Invalid direction")
  | ('F', _) ->
    (match from_dir with
    | "right" -> navigate_pipe (acc + 1) grid x (y + 1) c x y path
    | "below" -> navigate_pipe (acc + 1) grid (x + 1) y c x y path
    | _ -> failwith "Invalid direction")
  | ('7', _) ->
    (match from_dir with
    | "left" -> navigate_pipe (acc + 1) grid x (y + 1) c x y path
    | "below" -> navigate_pipe (acc + 1) grid (x - 1) y c x y path
    | _ -> failwith "Invalid direction")
  | ('|', _) ->
    (match from_dir with
    | "above" -> navigate_pipe (acc + 1) grid x (y + 1) c x y path
    | "below" -> navigate_pipe (acc + 1) grid x (y - 1) c x y path
    | _ -> failwith "Invalid direction")
  | ('L', _) ->
    (match from_dir with
    | "right" -> navigate_pipe (acc + 1) grid x (y - 1) c x y path
    | "above" -> navigate_pipe (acc + 1) grid (x + 1) y c x y path
    | _ -> failwith "Invalid direction")
  | ('-', _) ->
    (match from_dir with
    | "left" -> navigate_pipe (acc + 1) grid (x + 1) y c x y path
    | "right" -> navigate_pipe (acc + 1) grid (x - 1) y c x y path
    | _ -> failwith "Invalid direction")
  | (_, _) -> 
    printf "Found something else %c - %c\n" c prev_c;
    path
   ;;

let path = ref CoordBag.empty;;

(* let flood_fill grid (x, y) visited =
  let height = Array.length grid in
  printf "height: %d\n" height;
  let width = Array.length grid.(0) in
  printf "width: %d\n" width;
  let rec fill x y =
    if x < 0 || y < 0 || x >= width || y >= height || visited.(y).(x) then
      ()
    else match grid.(y).(x) with
    | 'x' -> ()  (* Skip path tiles *)
    | _ ->
      visited.(y).(x) <- true;
      fill (x + 1) y;  (* Right *)
      fill (x - 1) y;  (* Left *)
      fill x (y + 1);  (* Down *)
      fill x (y - 1);  (* Up *)
    in 
  fill x y *)


(* let count_tiles_inside_loop expanded_grid =
  let height = Array.length expanded_grid in
  let width = Array.length expanded_grid.(0) in
  let visited = Array.make_matrix ~dimx:height ~dimy:width false in
  flood_fill expanded_grid (0, 0) visited;
  let count = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if not visited.(y).(x) then
        match expanded_grid.(y).(x) with
        | 'x' -> ()  (* Skip path tiles *)
        | _ -> incr count
    done
  done;
  (!count, visited) *)

(* let get_inside_tiles visited_tiles expanded_grid path =
  let height = Array.length visited_tiles in
  let width = Array.length visited_tiles.(0) in
  let inside_tiles = ref CoordBag.empty in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if not visited_tiles.(y).(x) && not (Set.mem path (x, y)) then
        match expanded_grid.(y).(x) with
        | 'x' -> ()  (* Skip path tiles *)
        | _ -> inside_tiles := Set.add !inside_tiles (x, y)
    done
  done;
  !inside_tiles *)
  
  

(* let print_expanded_grid_with_pink_inside expanded_grid inside_tiles =
  let pink = "\027[95m" in  (* ANSI escape code for light red, which appears as pink in many terminals *)
  let reset = "\027[0m" in  (* ANSI escape code to reset the color *)
  Array.iteri expanded_grid ~f:(fun y row ->
    Array.iteri row ~f:(fun x c -> 
      if Set.mem inside_tiles (x, y) then
        Printf.printf "%s%c%s" pink c reset
      else
        Printf.printf "%c" c
    );
    Printf.printf "\n"
  )
;;  *)

let flood_fill grid (x, y) = 
  printf "(%d, %d) = %c\n" x y grid.(y).(x);
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let outside = ref CoordBag.empty in
  let path = ref CoordBag.empty in
  printf "HxW: %dx%d\n" height width;
  let rec fill x y =
    match (x, y) with
    | (x, y) when x < 0 || y < 0 || x >= width || y >= height -> 
      (* printf "out of bounds\n"; *)
      ()
    | (x, y) ->
      match (grid.(y).(x)) with
      | 'x' -> 
        (* printf "on the path (%d, %d)\n" x y;  *)
        path := Set.add !path (x, y);
      (* | _ when (x,y) not in the set !outside *)
      | _ when Set.mem !outside (x, y) -> 
        (* printf "already visited (%d, %d)\n" x y; *)
        ()
      | _ ->
        (* printf "filling (%d, %d)\n" x y; *)
        outside := Set.add !outside (x, y);
        (* Right *)
        fill (x + 1) y; 
        (* Left *) 
        fill (x - 1) y;  
        (* Down *)
        fill x (y + 1); 
        (* Up *) 
        fill x (y - 1); 
    in
  fill x y;
  !outside
  ;;

  let is_touching_path path x y =
    Set.exists ~f:(fun (px, py) ->
      (abs(px - x) = 1 && py = y) ||  (* Horizontal adjacent *)
      (abs(py - y) = 1 && px = x) ||  (* Vertical adjacent *)
      (abs(px - x) = 1 && abs(py - y) = 1)  (* Diagonal adjacent *)
    ) path
  

(* let display_flooded_grid grid outside_tiles path =
  let blue = "\027[34m" in   (* ANSI escape code for blue *)
  let yellow = "\027[33m" in (* ANSI escape code for yellow *)
  let green = "\027[32m" in  (* ANSI escape code for green *)
  let red = "\027[31m" in    (* ANSI escape code for red *)
  let reset = "\027[0m" in   (* ANSI escape code to reset the color *)
  Array.iteri grid ~f:(fun y row ->
    Array.iteri row ~f:(fun x _ ->
      if Set.mem path (x, y) then
        Printf.printf "%s#%s" blue reset
      else if is_touching_path path x y then
        Printf.printf "%s~%s" yellow reset
      else if Set.mem outside_tiles (x, y) then
        Printf.printf "%s_%s" green reset
      else
        Printf.printf "%s@%s" red reset
    );
    Printf.printf "\n"
  )
;; *)

let count_interior_tiles grid outside_tiles path =
  let count = ref 0 in
  Array.iteri grid ~f:(fun y row ->
    Array.iteri row ~f:(fun x _ ->
      if not (Set.mem path (x, y)) &&
         not (is_touching_path path x y) &&
         not (Set.mem outside_tiles (x, y)) then
        incr count  (* Counting the '@' symbol occurrences *)
    )
  );
  !count
;;
  
let find_expanded_path grid =
  let path_coords = ref CoordBag.empty in
  Array.iteri grid ~f:(fun y row ->
    Array.iteri row ~f:(fun x c ->
      match c with
      | 'x' -> path_coords := Set.add !path_coords (x, y)
      | _ -> ()
    )
  );
  !path_coords
  
let () = 
  printf "Original: \n";
  print_grid grid;  
  let (start_x, start_y) = find_start grid in
  let prev_c = '^' in
  let ans = navigate_pipe 0 grid start_x start_y prev_c start_x start_y path in 
  let path_grid = map_path grid !ans in
  print_grid path_grid;

  let expanded_grid = expand_map path_grid in
  print_grid expanded_grid;

  let outside_tiles = flood_fill expanded_grid (0, 0) in
  printf "outside_tiles: %d\n" (Set.length outside_tiles);

  let path_tiles = find_expanded_path expanded_grid in

  (* display_flooded_grid expanded_grid outside_tiles path_tiles; *)

  let interior_tile_count = count_interior_tiles expanded_grid outside_tiles path_tiles in
  printf "interior_tile_count: %d\n" interior_tile_count;

  match (interior_tile_count) with
  | 0 -> printf "You're Lost\n"
  | score -> printf "10.2: %d\n" (score / 9)

  (* let (inside_tiles, visited_tiles) = count_tiles_inside_loop expanded_grid in
  printf "inside_tiles: %d\n" inside_tiles;
  let inside_tile_set = get_inside_tiles visited_tiles expanded_grid !ans in
  print_expanded_grid_with_pink_inside expanded_grid inside_tile_set; *)

  (* let ans = navigate_pipe 0 grid start_x start_y prev_c start_x start_y path in  *)
  (* print_path grid !ans; *)

  (* match (Set.length inside_tile_set) with
  | 0 -> printf "You're Lost\n"
  | score -> printf "9.2: %d\n" (score / 2) *)
  ;;

  (*
  dune build puzzle_10_2.exe && cat ../data/day-10-test | dune exec ./puzzle_10_2.exe  
  dune build puzzle_10_2.exe && dune exec ./puzzle_10_2.exe   
  *)
