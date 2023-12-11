open Core

let grid = In_channel.read_lines "../data/day-11"
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

module IntSet = Set.Make(struct
  type t = int
  let compare = compare
  let t_of_sexp = Int.t_of_sexp
  let sexp_of_t = Int.sexp_of_t
end)

let print_grid grid =
  Array.iter grid ~f:(fun row ->
    Array.iter row ~f:(fun c -> printf "%c" c);
    printf "\n"
  )
  ;;

let get_empty_cols grid =
  let empty_cols = ref IntSet.empty in
  Array.iteri grid.(0) ~f:(fun i _ ->
    if Array.for_all grid ~f:(fun row -> Char.equal row.(i) '.')
    then empty_cols := Set.add !empty_cols i
  );
  !empty_cols
  ;;

let get_empty_rows grid =
  let empty_rows = ref IntSet.empty in
  Array.iteri grid ~f:(fun i row ->
    if Array.for_all row ~f:(fun c -> Char.equal c '.')
    then empty_rows := Set.add !empty_rows i
  );
  !empty_rows
  ;;

let get_galaxies grid =
  let galaxies = ref [] in
  Array.iteri grid ~f:(fun y row ->
    Array.iteri row ~f:(fun x c ->
      if Char.equal c '#' then
        galaxies := (x, y) :: !galaxies
    )
  );
  !galaxies
  ;;

let range a b =
  if a = b then []
  else
    match (a < b) with
    | true -> List.range a b
    | false -> List.range b a
  ;;
  
let calc_special range offset e =
  let rec aux acc range e = 
    match range with
    | [] -> acc
    | hd :: tl ->
        if Set.mem e hd then aux (acc + 1 + offset) tl e
        else aux (acc + 1) tl e
  in
  aux 0 range e
  ;;
  

let calc_manhattan_distance (x1, y1) (x2, y2) offset ec er =
  (* printf "offset: %d\n" offset; *)
  let x_range = range x1 x2 in
  let y_range = range y1 y2 in
  let x_distance = calc_special x_range offset ec in
  let y_distance = calc_special y_range offset er in
  (* printf "x_range: %d -> %d = %d\n" x1 x2 (List.length x_range);
  printf "y_range: %d -> %d = %d\n" y1 y2 (List.length y_range);
  printf "x_distance: %d\n" x_distance;
  printf "y_distance: %d\n" y_distance; *)
  x_distance + y_distance
  ;;

let rec process_galaxy_pairs acc pairs offset ec er =
  match pairs with
  | [] -> acc
  | hd :: tl ->
      (* let distance_raw = calc_manhattan_distance (fst hd) (snd hd) 0 ec er in *)
      let distance = calc_manhattan_distance (fst hd) (snd hd) offset ec er in
      (* printf "pair: (%d, %d) (%d, %d): [%d] -> %d\n" (fst (fst hd)) (snd (fst hd)) (fst (snd hd)) (snd (snd hd)) distance_raw distance; *)
      let acc' = acc + distance in

      process_galaxy_pairs acc' tl offset ec er
  ;;

let rec get_pairs galaxies pairs =
  match galaxies with
  | [] -> pairs
  | hd :: tl ->
      let pairs' = List.map ~f:(fun galaxy -> (hd, galaxy)) tl in
      get_pairs tl (pairs' @ pairs)
  ;;

let sum_path_lengths grid offset =
  (* printf "width: %d\n" (Array.length grid.(0)); 
  printf "offset: %d\n" offset; *)
  let empty_rows = get_empty_rows grid in
  printf "empty rows: %d\n" (Set.length empty_rows);
  let empty_cols = get_empty_cols grid in
  printf "empty cols: %d\n" (Set.length empty_cols);
  let galaxies = get_galaxies grid in
  printf "galaxies: %d\n" (List.length galaxies);
  let pairs = get_pairs galaxies [] in
  printf "pairs: %d\n" (List.length pairs);
  process_galaxy_pairs 0 pairs offset empty_cols empty_rows;
  ;;

let () = 
  print_grid grid;  
  match sum_path_lengths grid 1 with
  | 0 -> printf "Wrong universe\n"
  | distance -> printf "11.1: %d\n" distance
  ;;
  (* million - 1 *)
  match sum_path_lengths grid 999999 with
  | 0 -> printf "Wrong universe\n"
  | distance -> printf "11.2: %d\n" distance
  ;;

  (*
  dune build puzzle_11_1.exe && cat ../data/day-11-test | dune exec ./puzzle_11_1.exe  
  dune build puzzle_11_1.exe && dune exec ./puzzle_11_1.exe   

  790194712336
  790195502522
  *)
