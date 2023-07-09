open Stdio
(* open Map *)

let data = Helper.read_lines "../data/day-8"

let rec show_matrix_row matrix row col =
  let dim = Array.length matrix in
  match col with
  | _ when col = dim -> print_newline; 
  | _ -> 
    let _ = printf "%d" matrix.(col).(row) in
    show_matrix_row matrix row (col + 1)
  ;;

let rec show_matrix matrix row =
  let dim = Array.length matrix in
  let col = 0 in
  match row with
  | _ when row = dim -> printf "\n";
  | _ -> 
     let _f = show_matrix_row matrix row col in
     printf "\n";
     show_matrix matrix (row + 1)
  ;;
    
let rec add_row_to_matrix matrix row x y =
  match row with 
  | "" -> matrix 
  | _ -> 
    let _ = matrix.(x).(y) <- (int_of_string (String.sub row 0 1)) in
    add_row_to_matrix matrix (String.sub row 1 (String.length row - 1)) (x+1) y 
;;

let rec read_matrix data idx forest =
  match data with
  | [] -> 
    forest 
  | _ -> 
    let f = add_row_to_matrix forest (List.hd data) 0 idx in
    read_matrix (List.tl data) (idx + 1) f
  ;;

let look_west forest x y =
  let this_tree = forest.(x).(y) in
  let x = x - 1 in
  (* keep substracting 1 until you hit an edge (x=0) or a blocking tree*)
  let rec look_west' forest x y acc =
    match x with
    | -1 -> acc
    | _ when forest.(x).(y) >= this_tree -> acc + 1
    | _ -> 
      look_west' forest (x - 1) y (acc + 1)
  in
  look_west' forest x y 0
;;

let look_east forest x y =
  let this_tree = forest.(x).(y) in
  let x = x + 1 in
  (* keep adding 1 until you hit an edge (x=0) or a blocking tree*)
  let rec look_east' forest x y acc =
    match x with
    | _ when x = (Array.length forest) -> acc
    | _ when forest.(x).(y) >= this_tree -> acc + 1
    | _ -> 
      look_east' forest (x + 1) y (acc + 1)
  in
  look_east' forest x y 0

let look_north forest x y =
  let this_tree = forest.(x).(y) in
  let y = y - 1 in
  (* keep substracting 1 until you hit an edge (y=0) or a blocking tree*)
  let rec look_north' forest x y acc =
    match y with
    | -1 -> 
      (* printf "x=%d y=%d acc=%d\n" x y acc; *)
      acc
    | _ when forest.(x).(y) >= this_tree -> acc + 1
    | _ -> 
      look_north' forest x (y - 1) (acc + 1)
  in
  look_north' forest x y 0

let look_south forest x y =
  let this_tree = forest.(x).(y) in
  let y = y + 1 in
  (* keep adding 1 until you hit an edge (y=0) or a blocking tree*)
  let rec look_south' forest x y acc =
    match y with
    | _ when y = (Array.length forest) -> acc
    | _ when forest.(x).(y) >= this_tree -> acc + 1
    | _ -> 
      look_south' forest x (y + 1) (acc + 1)
  in
  look_south' forest x y 0

let get_plot_view forest x y max =
  let west = look_west forest x y in
  let east = look_east forest x y in
  let north = look_north forest x y in
  let south = look_south forest x y in
  let view = west * east * north * south in
  match max with
  | _ when view > max -> 
    (* printf "(x=%d,y=%d)\tn=%d s=%d e=%d w=%d view=%d\n" x y north south east west view; *)
    view
  | _ -> max
;;

let find_best_plot forest =
  let dim = (Array.length forest) - 1 in
  let minx, miny, maxx, maxy, max = 1, 1, dim -1, dim -1, 0 in
  (* let minx, miny, maxx, maxy, max = 1, 1, 5, 5, (dim - dim) in *)
  let rec find_best_plot' forest x y max =
    (* printf "x=%d y=%d max=%d\n" x y max; *)
    match x with
    | _ when x = maxx && y = maxy -> 
      (* We're done *)
      get_plot_view forest x y max
    | _ when x = maxx ->
      (* jump down a row and reset the column *)
      find_best_plot' forest minx (y + 1) (get_plot_view forest x y max)
    | _ ->
      find_best_plot' forest (x + 1) y (get_plot_view forest x y max)
    in
  find_best_plot' forest minx miny max
;;

let arr_size = List.hd data |> String.length
let _ = printf "Grid will be %dx%d\n" arr_size arr_size

let forest = (Array.make_matrix arr_size arr_size 0)

let _ = read_matrix data 0 forest
let _f = if false then show_matrix forest 0

(* scan trees from (1, dim-1) to (dim-1, dim-1) edges wil return 0 *)
(* for each tree scan 4 dirs *)
let () = printf "Best scenic score: %d\n" (find_best_plot forest)

(*
dune build puzzle_8_2.exe && dune exec ./puzzle_8_2.exe   
*)  