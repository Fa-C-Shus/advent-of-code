open Core

module Map = struct
  type t = {
    left: String.t;
    right: String.t;
  }
end
;;

let nodes = Hashtbl.create (module String)

let update_hash_map hash_map input =
  let id = (String.sub input ~pos:0 ~len:3) in
  let left = (String.sub input ~pos:7 ~len:3) in
  let right = (String.sub input ~pos:12 ~len:3) in
  Hashtbl.add_exn hash_map ~key:id ~data:{ Map.left = left; right = right }
  ; hash_map
;;

(* let parse_node str =
  let id = (String.sub str ~pos:0 ~len:3) in
  printf "ID: %s\n" id;
  let left = (String.sub str ~pos:7 ~len:3) in
  let right = (String.sub str ~pos:12 ~len:3) in
  { CoordStruct.id = id; left = left; right = right }
;; *)

(* let find_node_by_id nodes id =
  List.filter nodes ~f:(fun coord -> coord.CoordStruct.id = id)
;; *)

(* let find_node nodes id =
  List.find_exn nodes ~f:(fun coord -> coord.CoordStruct.id = id) *)

let find_node nodes id =
  match Hashtbl.find nodes id with
  | Some node -> node
  | None -> { Map.left = ""; right = "" }

let display_hash_map hash_map =
  Hashtbl.iteri hash_map ~f:(fun ~key ~data ->
    printf "%s -> %s, %s\n" key data.Map.left data.Map.right
  )
  ;;

let rec process_instructions nodes current_node instructions idx moves =
  match current_node with 
  | "ZZZ" -> 
    printf "You're Home\n";
    moves
  | _ ->
    let mod_index = idx mod (String.length instructions) in
    printf "Idx: %d\n" mod_index;
    let next_move = String.sub instructions ~pos:mod_index ~len:1 in
    printf "Next Move: %s\n" next_move;
    let next_node = match next_move with
      | "L" -> (find_node nodes current_node).Map.left
      | "R" -> (find_node nodes current_node).Map.right
      | _ -> "ZZZ" in
    printf "Next Node: %s\n" next_node;
    let idx' = idx + 1 in
    (*
    let score' = idx + 1 in
    let moves' = moves + 1 in
    printf "Move: %d, Score: %d, Next Move: %s, Next Node: %s\n" moves' score' next_move next_node; *)
    process_instructions nodes next_node instructions idx' (moves + 1)
  ;;

let rec navigate_map nodes instructions = 
  match In_channel.input_line In_channel.stdin with
  | Some line when (String.is_empty instructions) -> 
    printf "Instructions -> %s\n" line;
    let instructions' = line in
    navigate_map nodes instructions'
  | Some line when (String.is_empty line) -> 
    (* ignore blank lines *)
    navigate_map nodes instructions
  | Some line -> 
    printf "Node -> %s\n" line;
    let nodes' = update_hash_map nodes line in
    printf "Len Nodes -> %d\n" (Hashtbl.length nodes');
    navigate_map nodes' instructions
  | None -> 
    (* End of input *)
    display_hash_map nodes;
    (* let current_node = find_node nodes "AAA" in *)
    process_instructions nodes "AAA" instructions 0 0
  ;;


let () = 
  match navigate_map nodes "" with
  | 0 -> printf "You're Lost\n"
  | score -> printf "Navigation Moves: %d\n" score

  (*
  dune build puzzle_08_1.exe && cat ../data/day-08-test | dune exec ./puzzle_08_1.exe   
  *)