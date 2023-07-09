open Stdio
(* open Map *)

(* Collect the trees as a set to not double count *)
module CoordBag = Set.Make (struct
  type t = int * int

  let compare = Stdlib.compare
end)

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

let find_visible_trees forest  =
  let dim = (Array.length forest) - 1 in
  let visible = ref CoordBag.empty in

  let rec find_trees_by_row forest visible col row max =
    (* printf "Checking col: %d, row: %d, height: %d max: %d\n" row col forest.(col).(row) max; *)
    match col with
    | _ when col = dim && row = dim -> 
      (* all outside trees are visible *)
      (* printf "Adding very last tree(%d, %d) with max: %d\n" col row max; *)
      visible := CoordBag.add (col, row) !visible;
      visible
    | _ when col = dim -> 
      (* all outside trees are visible *)
      (* printf "Adding last tree(%d, %d) with max: %d\n" col row max; *)
      visible := CoordBag.add (col, row) !visible;
      (* reset the max and col and bump the row *)
      find_trees_by_row forest visible 0 (row + 1) 0
    | 0 -> 
      (* all outside trees are visible *)
      visible := CoordBag.add (col, row) !visible;
      let max = forest.(col).(row) in
      (* printf "Adding first tree(%d, %d) with max: %d\n" col row max; *)
      find_trees_by_row forest visible (col + 1) row max
    | _ when forest.(col).(row) > max -> 
      visible := CoordBag.add (col, row) !visible;
      let max = forest.(col).(row) in
      (* printf "Adding inside tree(%d, %d) with max: %d\n" col row max; *)
      find_trees_by_row forest visible (col + 1) row max
    | _ -> 
      find_trees_by_row forest visible (col + 1) row max
    in 

  let rec find_trees_by_row_rev forest visible col row max =
    (* printf "Checking col: %d, row: %d, height: %d max: %d\n" row col forest.(col).(row) max; *)
    match col with
    | 0 when row < dim -> 
      (* all outside trees are visible *)
      (* printf "Adding first tree(%d, %d) with max: %d\n" col row max; *)
      visible := CoordBag.add (col, row) !visible;
      (* reset the max and col and bump the row *)
      find_trees_by_row_rev forest visible dim (row + 1) 0
    | _ when col = dim && row < dim -> 
      (* all outside trees are visible *)
      visible := CoordBag.add (col, row) !visible;
      let max = forest.(col).(row) in
      (* printf "Adding first tree(%d, %d) with max: %d\n" col row max; *)
      find_trees_by_row_rev forest visible (col - 1) row max
    | _ when col = 0 && row = dim -> 
      (* all outside trees are visible *)
      (* printf "Adding very last tree(%d, %d) with max: %d\n" col row max; *)
      visible := CoordBag.add (col, row) !visible;
      visible
    | _ when forest.(col).(row) > max -> 
      visible := CoordBag.add (col, row) !visible;
      let max = forest.(col).(row) in
      (* printf "Adding inside tree(%d, %d) with max: %d\n" col row max; *)
      find_trees_by_row_rev forest visible (col - 1) row max
    | _ -> 
      find_trees_by_row_rev forest visible (col - 1) row max
    in 

  let rec find_trees_by_col forest visible col row max =
    (* printf "Checking col: %d, row: %d, height: %d max: %d\n" row col forest.(col).(row) max; *)
    match row with
    | _ when col = dim && row = dim -> 
      (* all outside trees are visible *)
      (* printf "Adding very last tree(%d, %d) with max: %d\n" col row max; *)
      visible := CoordBag.add (col, row) !visible;
      visible
    | _ when row = dim -> 
      (* all outside trees are visible *)
      (* printf "Adding last tree(%d, %d) with max: %d\n" col row max; *)
      visible := CoordBag.add (col, row) !visible;
      (* reset the max and row and bump the col *)
      find_trees_by_col forest visible (col + 1) 0 0
    | 0 -> 
      (* all outside trees are visible *)
      visible := CoordBag.add (col, row) !visible;
      let max = forest.(col).(row) in
      (* printf "Adding first tree(%d, %d) with max: %d\n" col row max; *)
      find_trees_by_col forest visible col (row + 1) max
    | _ when forest.(col).(row) > max -> 
      visible := CoordBag.add (col, row) !visible;
      let max = forest.(col).(row) in
      (* printf "Adding inside tree(%d, %d) with max: %d\n" col row max; *)
      find_trees_by_col forest visible col (row + 1) max
    | _ -> 
      find_trees_by_col forest visible col (row + 1) max
    in 
      
  let rec find_trees_by_col_rev forest visible col row max =
    (* printf "Checking col: %d, row: %d, height: %d max: %d\n" row col forest.(col).(row) max; *)
    match row with
    | _ when col = dim && row = 0 -> 
      (* all outside trees are visible *)
      (* printf "Adding very last tree(%d, %d) with max: %d\n" col row max; *)
      visible := CoordBag.add (col, row) !visible;
      visible
    | _ when row = dim -> 
      (* all outside trees are visible *)
      visible := CoordBag.add (col, row) !visible;
      let max = forest.(col).(row) in
      (* printf "Adding first tree(%d, %d) with max: %d\n" col row max; *)
      find_trees_by_col_rev forest visible col (row - 1) max
    | 0 -> 
      (* all outside trees are visible *)
      (* printf "Adding last tree(%d, %d) with max: %d\n" col row max; *)
      visible := CoordBag.add (col, row) !visible;
      (* reset the max and row and bump the col *)
      find_trees_by_col_rev forest visible (col + 1) dim 0
    | _ when forest.(col).(row) > max -> 
      visible := CoordBag.add (col, row) !visible;
      let max = forest.(col).(row) in
      (* printf "Adding inside tree(%d, %d) with max: %d\n" col row max; *)
      find_trees_by_col_rev forest visible col (row - 1) max
    | _ -> 
      find_trees_by_col_rev forest visible col (row - 1) max
    in 
      

  let _ = find_trees_by_row forest visible 0 0 0 in
  let _ = find_trees_by_row_rev forest visible dim 0 0 in
  let _ = find_trees_by_col forest visible 0 0 0 in
  let _ = find_trees_by_col_rev forest visible 0 dim 0 in
  Some !visible
  ;;
  


let arr_size = List.hd data |> String.length
let _ = printf "Grid will be %dx%d\n" arr_size arr_size

let forest = (Array.make_matrix arr_size arr_size 0)

let _ = read_matrix data 0 forest
let _f = if false then show_matrix forest 0


let () = 
match find_visible_trees forest with 
| None -> printf "No trees found\n"
| Some trees -> printf "Visible trees found: %d\n" (CoordBag.cardinal trees)

(*
dune build puzzle_8_1.exe && dune exec ./puzzle_8_1.exe   
*)  