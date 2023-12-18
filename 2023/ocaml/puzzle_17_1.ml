open Core
open Helper

type coord = int * int  (* (x, y) coordinates *)

type path_node = int * int * int * coord * int
(* let equal_path_node (a1, b1, c1, (x1, y1), d1, _) (a2, b2, c2, (x2, y2), d2, _) =
  a1 = a2 && b1 = b2 && c1 = c2 && x1 = x2 && y1 = y2 && d1 = d2
;; *)
(* let print_path_node (a, b, c, (x, y), d, _) =
  Printf.printf "(%d, %d, %d, (%d, %d), %d)\n" a b c x y d
;; *)
(* let path_node_to_visited_item (_first, a, b, c, d) : VisitedItem.t =
  (a, b, c, d)
;; *)

module PathNodeOrd : ORDERED with type t = path_node = struct
  type t = path_node
  let compare (total1, _, _, _, _) (total2, _, _, _, _) =
    Stdlib.compare total1 total2
end

module VisitedItem = struct
  type t = int * int * coord * int
  let compare = Stdlib.compare

  (* Dummy implementations for sexp conversion functions *)
  let sexp_of_t (_: t) = failwith "Not implemented"
  let t_of_sexp (_: Sexplib0.Sexp.t) = failwith "Not implemented"
end

module VisitedItemSet = Set.Make(VisitedItem)
module PathNodeHeap = MinHeap(PathNodeOrd)

let digit_char_to_int c =
  Char.to_int c - Char.to_int '0'
;;

let filename = "../data/day-17";;

let grid = In_channel.read_lines filename
  |> List.map ~f:(fun line -> String.to_list line)
  |> Array.of_list_map ~f:Array.of_list
;;

let up : coord = (-1, 0);;
let down : coord = (1, 0);;
let left : coord = (0, -1);;
let right : coord = (0, 1);;

let directions = [up; down; left; right];;

(* let print_grid grid =
  Array.iter grid ~f:(fun row ->
    Array.iter row ~f:(fun c -> printf "%c" c);
    printf "\n"
  );
  printf "\n"
;; *)

(* let rec pop_path (paths_to_explore : path_node list) list_idx min_path min_node =
  match (list_idx = List.length paths_to_explore) with
  | true -> 
    (* remove the min node from the list *)
    let paths_to_explore' = 
      List.filter paths_to_explore ~f:(fun node -> not (equal_path_node node min_node)) in
    (* return the min node, and new list *)
    (min_node, paths_to_explore')
  | false ->
    let ((total: int), (x: int), (y: int), (dir_moved: coord), (curr_dir_len: int), path) = 
      List.nth_exn paths_to_explore list_idx in
    if total < min_path then
      pop_path paths_to_explore (list_idx + 1) total (total, x, y, dir_moved, curr_dir_len, path)
    else if total = min_path then
      let (_, _, y', _, _, _) = min_node in
      (* printf "y: %d, curr_dir_len: %d\n" y curr_dir_len; *)
      if y > y' then
        pop_path paths_to_explore (list_idx + 1) total (total, x, y, dir_moved, curr_dir_len, path)
      else 
        pop_path paths_to_explore (list_idx + 1) min_path min_node
    else
      pop_path paths_to_explore (list_idx + 1) min_path min_node
;; *)

let navigate grid min_dir_moves max_dir_moves =
  let target =
    (Array.length grid - 1, Array.length grid.(0) - 1) 
  in
  (* printf "I need to navigate to (%d, %d)\n" (fst target) (snd target); *)
  let visited = VisitedItemSet.empty in
  (* let paths_to_explore : path_node list = [(0,0,0,right,1,[]);(0,0,0,down,1,[])] in *)
  let paths_to_explore = PathNodeHeap.empty in
  let paths_to_explore = PathNodeHeap.insert paths_to_explore (0,0,0,right,1) in
  let paths_to_explore = PathNodeHeap.insert paths_to_explore (0,0,0,down,1) in
  (* printf "min_dir_moves: %d, max_dir_moves: %d\n" min_dir_moves max_dir_moves; *)

  let rec navigate_remaining_paths paths_to_explore visited =
    match (PathNodeHeap.is_empty paths_to_explore) with
    | true -> 
      (* printf "length of visited: %d\n" (Set.length visited); *)
      0
    | _ -> 
      (* Heap.pop *)
      (* let ((min_node: path_node), paths_to_explore') = 
        pop_path paths_to_explore 0 Int.max_value (Int.max_value, 0, 0, (0,0), 0, []) in *)
      let (min_node: path_node) = 
        match (PathNodeHeap.find_min paths_to_explore) with 
        | None -> failwith "No min node"
        | Some node -> node
      in
      let paths_to_explore' = 
        match (PathNodeHeap.delete_min paths_to_explore) with 
        | None -> failwith "No min node"
        | Some heap -> heap
      in
      
        (* printf "\tmin_node: ";
        print_path_node min_node; *)
      let (total, x, y, dir_moved, curr_dir_len) = min_node in
      (* printf "path Count: %d (%d, %d) -> %d\n" (List.length paths_to_explore) x y total; *)
      (* printf "\ttotal: %d\n" total; *)
      let visit_node = (x, y, dir_moved, curr_dir_len) in

      (* Check if we've visited this node *)
      let been_here = Set.mem visited visit_node in
      (* printf "\tbeen_here: %b\n" been_here; *)

      match been_here with
      | true -> navigate_remaining_paths paths_to_explore' visited
      | false ->
        (* Add this node to visited *)
        let visited' = Set.add visited visit_node in

        let dir_exceeded = (curr_dir_len > max_dir_moves) in
        (* printf "\tdir_exceeded: %b\tc: %d\tm: %d\n" dir_exceeded curr_dir_len max_dir_moves; *)

        match dir_exceeded with
        | true -> 
          navigate_remaining_paths paths_to_explore' visited'
        | false ->
          let new_coord = (x + (fst dir_moved), y + (snd dir_moved)) in
          (* printf "\tnew_coord: (%d, %d)\n" (fst new_coord) (snd new_coord); *)

          let oob = (fst new_coord < 0 || fst new_coord >= Array.length grid) ||
                    (snd new_coord < 0 || snd new_coord >= Array.length grid.(0)) in
          (* printf "\toob: %b\n" oob; *)

          match oob with
          | true -> 
            navigate_remaining_paths paths_to_explore' visited'
          | false ->
            let loc_val = grid.(fst new_coord).(snd new_coord) in
            let total' = total + (digit_char_to_int loc_val) in
            (* printf "\t\tbump total': %d\n" total'; *)

            let min_dir_exceeded = (curr_dir_len > min_dir_moves) in
            (* printf "\tmin_dir_exceeded: %b\tc: %d\tm: %d\n" min_dir_exceeded curr_dir_len min_dir_moves; *)

            let at_target = ((fst new_coord) = (fst target) && (snd new_coord) = (snd target)) in
            (* printf "\tat_target: %b\n" at_target; *)

            match (min_dir_exceeded && at_target) with
            | true -> 
              printf "Found target!\n";
              (* printf "Length Path: %d\n" (List.length curr_path);
              printf "Path: ";
              List.iter curr_path ~f:(fun (x, y) -> printf "(%d, %d) " x y);
              printf "\n"; *)
              total'
            | false ->
              (* I can't return to last cell *)
              (* if I am less than min moves I can only continue in the direction I was going *)
              let rec get_next_nodes dir_idx nodes =
                match dir_idx = List.length directions with
                | true -> nodes
                | false ->
                  let (dir: coord) = List.nth_exn directions dir_idx in
                  let just_there = (fst dir) + (fst dir_moved) = 0 && (snd dir) + (snd dir_moved) = 0 in
                  let same_dir = (fst dir) = (fst dir_moved) && (snd dir) = (snd dir_moved) in
                  (* printf "\tSame Dir: %b\n" same_dir; *)
                  (* printf "\t\tjust_there: %b\n" just_there; *)
                  (* match ((not just_there) || curr_dir_len < min_dir_moves) with *)
                  match (just_there) with
                  | true -> 
                    get_next_nodes (dir_idx + 1) nodes
                  | false when (not same_dir) && curr_dir_len < min_dir_moves -> 
                    (* printf "\t\t\tcan't turn yet\n"; *)
                    get_next_nodes (dir_idx + 1) nodes
                  | false ->
                    let new_dir_len = 
                      match ((fst dir) = (fst dir_moved) && (snd dir) = (snd dir_moved)) with
                      | true -> curr_dir_len + 1
                      | false -> 1
                    in
                    (* let curr_path' = List.append curr_path [new_coord] in *)
                    let next_node = (total', fst new_coord, snd new_coord, dir, new_dir_len) in
                    let nodes' = List.append nodes [next_node] in
                    get_next_nodes (dir_idx + 1) nodes'
                in
              let next_nodes = get_next_nodes 0 [] in
              (* 
                iterate over the next_nodes;
                foreach node, insert it into the MinHeap(paths_to_explore)
                 *)
              let paths_to_explore'' = List.fold next_nodes ~init:paths_to_explore' ~f:(fun heap node ->
                PathNodeHeap.insert heap node
              ) in
              (* let paths_to_explore'' = List.append paths_to_explore' next_nodes in *)

              navigate_remaining_paths paths_to_explore'' visited'
  in
  navigate_remaining_paths paths_to_explore visited
;;

let () =
  (* print_grid grid; *)
  let puzzle1 = navigate grid 0 3 in 
  printf "17.1: %d\n" puzzle1;
  
  let puzzle2 = navigate grid 4 10 in
  printf "17.2: %d\n" puzzle2;
  (* match (navigate grid 0 3) with
  | 0 -> printf "You're Lost\n"
  | score -> printf "17.1: %d\n" score;; *)

  (* match (navigate grid 4 10) with
  | 0 -> printf "You're Lost\n"
  | score -> printf "17.2: %d\n" score;; *)
;;

(*
  dune build puzzle_17_1.exe && dune exec ./puzzle_17_1.exe   
*)