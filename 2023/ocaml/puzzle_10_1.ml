open Core

let grid = In_channel.read_lines "../data/day-10"
  |> List.map ~f:(fun line -> String.to_list line)
  |> Array.of_list_map ~f:Array.of_list
  ;;

let print_grid grid =
  Array.iter grid ~f:(fun row ->
    Array.iter row ~f:(fun c -> printf "%c" c);
    printf "\n"
  )
  ;;

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
  printf "left: %c, right: %c, up: %c, down: %c\n" 
    (Option.value left' ~default:'x')
    (Option.value right' ~default:'x')
    (Option.value up' ~default:'x')
    (Option.value down' ~default:'x');
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
let rec navigate_pipe acc grid x y prev_c x1 y1 = 
  let c = grid.(y).(x) in
  printf "char: %c\n" c;
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
  printf "from_dir %s\n" from_dir;
  match (c, prev_c) with
  | ('S', '^') -> 
    (* printf "Found start; determine what I should have been\n";
    printf "grid size: %d, %d\n" (Array.length grid.(0)) (Array.length grid); *)
    let (shadow_start, next_x, next_y) = (get_shadow_start grid x y) in
    (* printf "shadow_start: %c, %d, %d\n" shadow_start next_x next_y; *)
    navigate_pipe (acc + 1) grid next_x next_y shadow_start x1 y1
  | ('S', _) -> 
    printf "Found end\n";
    acc
  | ('J', _) ->
    (match from_dir with
    | "left" -> navigate_pipe (acc + 1) grid x (y - 1) c x y
    | "above" -> navigate_pipe (acc + 1) grid (x - 1) y c x y
    | _ -> failwith "Invalid direction")
  | ('F', _) ->
    (match from_dir with
    | "right" -> navigate_pipe (acc + 1) grid x (y + 1) c x y
    | "below" -> navigate_pipe (acc + 1) grid (x + 1) y c x y
    | _ -> failwith "Invalid direction")
  | ('7', _) ->
    (match from_dir with
    | "left" -> navigate_pipe (acc + 1) grid x (y + 1) c x y
    | "below" -> navigate_pipe (acc + 1) grid (x - 1) y c x y
    | _ -> failwith "Invalid direction")
  | ('|', _) ->
    (match from_dir with
    | "above" -> navigate_pipe (acc + 1) grid x (y + 1) c x y
    | "below" -> navigate_pipe (acc + 1) grid x (y - 1) c x y
    | _ -> failwith "Invalid direction")
  | ('L', _) ->
    (match from_dir with
    | "right" -> navigate_pipe (acc + 1) grid x (y - 1) c x y
    | "above" -> navigate_pipe (acc + 1) grid (x + 1) y c x y
    | _ -> failwith "Invalid direction")
  | ('-', _) ->
    (match from_dir with
    | "left" -> navigate_pipe (acc + 1) grid (x + 1) y c x y
    | "right" -> navigate_pipe (acc + 1) grid (x - 1) y c x y
    | _ -> failwith "Invalid direction")
  | (_, _) -> 
    printf "Found something else %c - %c\n" c prev_c;
    45
 
  ;;

let () = 
  print_grid grid;  
  let (start_x, start_y) = find_start grid in
  printf "Start: %d, %d\n" start_x start_y;
  let prev_c = '^' in
  match navigate_pipe 0 grid start_x start_y prev_c start_x start_y with
  | 0 -> printf "You're Lost\n"
  | score -> printf "9.1: %d\n" (score / 2)
  ;;

  (*
  dune build puzzle_10_1.exe && cat ../data/day-10-test | dune exec ./puzzle_10_1.exe  
  dune build puzzle_10_1.exe && dune exec ./puzzle_10_1.exe   
  *)
