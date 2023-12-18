open Core

(* (x, y) coordinates *)
type coord = int * int

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

let display_path path ignore =
  let rec display_path' path =
    match path with
    | [] -> printf "\n"
    | (r, c) :: t -> 
      printf "(%d, %d) " r c;
      display_path' t
  in
  match ignore with
  | true -> ()
  | false ->
    display_path' path
;;

let find_start grid =
  (* iterate over the grid until you find the character S *)
  let rec find_start' grid pos =
    let (x, y) = pos in
    match grid.(y).(x) with
    | 'S' -> (x, y)
    | _ -> 
      match (x = Array.length grid.(y) - 1) with 
      | true -> find_start' grid (0, (y + 1))
      | false -> find_start' grid ((x + 1), y)
  in
  find_start' grid ((0, 0): coord)
;;

let get_shadow_start grid pos =
  let width = Array.length grid.(0) in
  let height = Array.length grid in
  let (x, y) = pos in
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
  match (left', right', up', down') with
  | (Some _, Some _, None, None) -> ('-', (x + 1, y))
  | (Some _, None, Some _, None) -> ('J', (x, y - 1))
  | (Some _, None, None, Some _) -> ('7', (x - 1 , y))
  | (None, Some _, Some _, None) -> ('L', (x, y - 1))
  | (None, Some _, None, Some _) -> ('F', (x + 1, y))
  | (None, None, Some _, Some _) -> ('|', (x, y - 1))
  | _ -> failwith "shit ain'y right"
;;  

let shoelace_and_pick path_len vertices =
  let vert_size = List.length vertices in
  let max_index = vert_size - 1 in
  let rec shoelace_and_pick' acc index =
    match (index = vert_size) with
    | true -> 
      (* acc is the interior amount *)
      (* picks theorem *)
      display_path vertices true;
      (* printf "sum: %d\n" acc; *)
      (* divide by 2 because we have added the entire width *)
      let shoelace = abs(acc) / 2 in
      (* printf "shoelace: %d\n" shoelace; *)
      (* printf "path_tiles: %d\n" path_tiles; *)
      let pick = shoelace - path_len / 2 + 1 in
      (* printf "pick: %d\n" pick; *)
      (pick)
    | false ->
      let prev_index = 
        match index - 1 with
        | -1 -> max_index
        | _ -> index - 1
      in
      let next_index = 
        match index + 1 with
        | x when x > max_index -> 0
        | x -> x
      in

      let get_coord idx =
        match (List.nth vertices idx) with
        | Some good -> good
        | None -> failwith "Whiskey Tango Firetruck"
      in

      (* printf "index: %d, prev_index: %d, next_index: %d\n" index prev_index next_index; *)
      (* get the current point *)
      let (r, _) = get_coord index in
      (* get the previous point using modulo of vertices size *)
      let (_, cp) = get_coord prev_index in
      (* get the next point using modulo of vertices size *)
      let (_, cn) = get_coord next_index in

      (* 
        Apply shoelace theorem 
        https://en.wikipedia.org/wiki/Shoelace_formula
        r * cp - cn
      *)
      let this_part = r * (cp - cn) in
      (* printf "i:: %d\t%d\n" index this_part; *)
      let acc' = acc + this_part in
      shoelace_and_pick' acc' (index + 1)
  in
  shoelace_and_pick' 0 0
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
let navigate_pipe grid = 
  let (start_pos: coord) = find_start grid in
  let (start_symbol, next_pos) = get_shadow_start grid start_pos in
  let empty = [] in
  let vertices = 
    match start_symbol with
    (* the starting position is a turn *)
    | 'L' | 'J' | 'F' | '7' -> empty @ [start_pos]
    (* the starting position is a straightaway *)
    | _ -> []
  in

  let rec navigate_pipe' grid (curr_pos: coord) (prev_pos: coord) prev_c vertices path_len =
    let (x, y) = curr_pos in
    let (x1, y1) = prev_pos in
    let c = grid.(y).(x) in
    let dx = x - x1 in
    let dy = y - y1 in 
    (* printf "prev_c: %c, curr_c: %c\n" prev_c c;
    printf "total length: %d\n" path_len;
    printf "c: %c with delta: %d, %d\n" c dx dy;
    printf "Vertices: %s\n" (List.to_string vertices ~f:(fun (x, y) -> sprintf "(%d, %d)" x y)); *)
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
    | ('S', _) -> 
      printf "Found end\n";
      shoelace_and_pick path_len vertices
    | ('J', _) ->
      (match from_dir with
      | "left" -> navigate_pipe' grid (x, y - 1) curr_pos c (vertices @ [curr_pos]) (path_len + 1) 
      | "above" -> navigate_pipe' grid (x - 1, y) curr_pos c (vertices @ [curr_pos]) (path_len + 1) 
      | _ -> failwith "Invalid direction")
    | ('F', _) ->
      (match from_dir with
      | "right" -> navigate_pipe' grid (x, y + 1) curr_pos c (vertices @ [curr_pos]) (path_len + 1) 
      | "below" -> navigate_pipe' grid (x + 1, y) curr_pos c (vertices @ [curr_pos]) (path_len + 1) 
      | _ -> failwith "Invalid direction")
    | ('7', _) ->
      (match from_dir with
      | "left" -> navigate_pipe' grid (x, y + 1) curr_pos c (vertices @ [curr_pos]) (path_len + 1)
      | "below" -> navigate_pipe' grid (x - 1, y) curr_pos c (vertices @ [curr_pos]) (path_len + 1) 
      | _ -> failwith "Invalid direction")
    | ('L', _) ->
      (match from_dir with
      | "right" -> navigate_pipe' grid (x, y - 1) curr_pos c (vertices @ [curr_pos]) (path_len + 1) 
      | "above" -> navigate_pipe' grid (x + 1, y) curr_pos c (vertices @ [curr_pos]) (path_len + 1) 
      | _ -> failwith "Invalid direction")
    | ('-', _) ->
      (match from_dir with
      | "left" -> navigate_pipe' grid (x + 1, y) curr_pos c vertices (path_len + 1)
      | "right" -> navigate_pipe' grid (x - 1, y) curr_pos c vertices (path_len + 1)
      | _ -> failwith "Invalid direction")
    | ('|', _) ->
      (match from_dir with
      | "above" -> navigate_pipe' grid (x, y + 1) curr_pos c vertices (path_len + 1)
      | "below" -> navigate_pipe' grid (x, y - 1) curr_pos c vertices (path_len + 1)
      | _ -> failwith "Invalid direction")
    | (_, _) ->
      printf "Found something else %c - %c\n" c prev_c;
      path_len
  in
  navigate_pipe' grid next_pos start_pos 'S' vertices 1 
;;

let () = 
  printf "Original: \n";
  print_grid grid;  
  let answer = navigate_pipe grid in
  printf "Answer: %d\n" answer;
  ;;

  (*
  dune build puzzle_10_2_alt.exe && cat ../data/day-10-test | dune exec ./puzzle_10_2_alt.exe  
  dune build puzzle_10_2_alt.exe && dune exec ./puzzle_10_2_alt.exe   
  *)
