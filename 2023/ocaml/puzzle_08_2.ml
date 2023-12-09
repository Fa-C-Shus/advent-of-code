open Printf

module StringMap = Map.Make(String)

let gcd a b =
  let rec aux a b =
    if b = 0 then a else aux b (a mod b)
  in
  aux (abs a) (abs b)

let lcm a b =
  match a, b with
  | 0, _ | _, 0 -> 0
  | _ -> abs (a * b) / gcd a b

let list_lcm lst =
  List.fold_left lcm 1 lst

let rec follow_this_route left_routes right_routes instructions current_node moves = 
  match Helper.ends_with current_node "Z" with
  | true -> moves
  | _ ->
    (* do the next move *)
    let direction = String.sub instructions (moves mod (String.length instructions)) 1 in
    let next_node = 
      match direction with
      | "L" -> StringMap.find current_node left_routes
      | "R" -> StringMap.find current_node right_routes
      | _ -> failwith "Unknown Direction"
    in
    follow_this_route left_routes right_routes instructions next_node (moves + 1)
  ;;

let rec follow_the_routes left_routes right_routes instructions starters start_index moves_list = 
  match starters with 
  | [] -> 
    printf "Moves List: %d\n" (List.length moves_list);
    list_lcm moves_list
  | current_node :: rest ->
    printf "Current Node: %s\n" current_node;
    let route_moves = follow_this_route left_routes right_routes instructions current_node 0 in
    printf "Route Moves: %d\n" route_moves;
    let moves_list' = List.append moves_list [route_moves] in
    follow_the_routes left_routes right_routes instructions rest (start_index + 1) moves_list'
  ;;

let rec navigate_map left_routes right_routes instructions starters = 
  match In_channel.input_line In_channel.stdin with
  | Some line when (String.length instructions) = 0 -> 
    (* The first line is the instructions *)
    let instructions' = line in
    navigate_map left_routes right_routes instructions' starters
  | Some line when (String.length line) = 0 -> 
    (* ignore blank lines *)
    navigate_map left_routes right_routes instructions starters
  | Some line -> 
    (* printf "Node -> %s\n" line; *)
    let src = String.sub line 0 3 in
    let left = String.sub line 7 3 in
    let right = String.sub line 12 3 in
    (* printf "Src: %s Left: %s Right: %s\n" src left right; *)
    let left_routes' = (StringMap.add src left left_routes) in
    let right_routes' = (StringMap.add src right right_routes) in
    if Helper.ends_with src "A" then
      let starters' = List.append starters [src] in
      navigate_map left_routes' right_routes' instructions starters'
    else
      navigate_map left_routes' right_routes' instructions starters
  | None -> 
    (* End of input *)
    printf "Instructions: %s\n" instructions;
    printf "Start Nodes : %d\n" List.(length starters);
    follow_the_routes left_routes right_routes instructions starters 0 []
    (* process_starts nodes start_nodes instructions 0 0 [] *)
    (* process_instructions nodes start_nodes instructions 0 0 *)
  ;;

let () = 
  let go_left = StringMap.empty in 
  let go_right = StringMap.empty in

  match navigate_map go_left go_right "" [] with
  | 0 -> printf "You're Lost\n"
  | score -> printf "8.2: %d\n" score

  (*
  dune build puzzle_08_2.exe && cat ../data/day-08-test | dune exec ./puzzle_08_2.exe   
  *)