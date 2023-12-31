open Printf

(* (x, y) coordinates *)
type coord = int * int

let up : coord = (-1, 0);;
let down : coord = (1, 0);;
let left : coord = (0, -1);;
let right : coord = (0, 1);;

(* Create a hash table *)
let directions = Hashtbl.create 4;;

(* Add items to the hash table *)
let () =
  Hashtbl.add directions "U" up;
  Hashtbl.add directions "D" down;
  Hashtbl.add directions "L" left;
  Hashtbl.add directions "R" right;
;;

(* Custom function to split a string by spaces *)
let split_on_space s =
  Str.split (Str.regexp " +") s
;;

(* Function to convert char to int *)
(* let digit_char_to_int c =
  Char.code c - Char.code '0' *)

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

let shoelace_and_pick path_tiles path =
  let path_size = List.length path in
  let max_index = path_size - 1 in
  let rec shoelace_and_pick' acc index =
    match (index = path_size) with
    | true -> 
      (* acc is the interior amount *)
      (* picks theorem *)
      display_path path true;
      (* printf "sum: %d\n" acc; *)
      (* divide by 2 because we have added the entire width *)
      let shoelace = abs(acc) / 2 in
      (* printf "shoelace: %d\n" shoelace; *)
      (* printf "path_tiles: %d\n" path_tiles; *)
      let pick = shoelace - path_tiles / 2 + 1 in
      (* printf "pick: %d\n" pick; *)
      (path_tiles + pick)
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
      (* printf "index: %d, prev_index: %d, next_index: %d\n" index prev_index next_index; *)
      (* get the current point *)
      let (r, _) = List.nth path index in
      (* get the previous point using modulo of path size *)
      let (_, cp) = List.nth path prev_index in
      (* get the next point using modulo of path size *)
      let (_, cn) = List.nth path next_index in

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



let dig =
  let empty_path = [] in
  let rec dig' acc path = 
    match In_channel.input_line In_channel.stdin with
    | None -> 
      printf "Path length: %d\n" (List.length path);
      shoelace_and_pick acc path
    | Some line -> 
      let line' = split_on_space line in
      let direction = List.nth line' 0 in
      let steps = List.nth line' 1 |> int_of_string in
      (* printf "direction: %s, steps: %d\n" direction steps; *)

      (* let d = Hashtbl.find directions direction in *)
      let (dr, dc) = Hashtbl.find directions direction in
      (* printf "d: %d, %d\n" dr dc; *)

      (* get the last element of the path *)
      let acc' = acc + steps in

      (* get the last element of the path *)
      let (r, c) = List.hd (List.rev path) in
      let path' = path @ [((r + dr * steps), c + dc * steps)] in

      dig' acc' path'
  in
  let path = empty_path @ [(0, 0)] in
  dig' 0 path
;;

let () =
  let puzzle1 = dig in 
  printf "18.1: %d\n" puzzle1;
;;

(*
  dune build puzzle_18_1.exe && cat ../data/day-18-test | dune exec ./puzzle_18_1.exe   
*)