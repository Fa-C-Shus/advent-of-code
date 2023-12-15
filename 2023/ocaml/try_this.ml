open Core

let rotate grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in

  (* Create a new grid with transposed dimensions *)
  let new_grid = Array.init cols ~f:(fun _ -> Array.create ~len:rows '?') in

  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      new_grid.(c).(rows - 1 - r) <- grid.(r).(c)
    done
  done;

  new_grid
;;

let print_grid grid =
  Array.iter ~f:(fun row ->
    Array.iter ~f:(Printf.printf "%c") row;
    Printf.printf "\n"
  ) grid
;;

let () =
  (* Example initialization of a grid *)
  let grid = [|
    [|'.'; 'O'; '.'; 'O'|];
    [|'O'; '.'; 'O'; '.'|];
    [|'.'; 'O'; '.'; 'O'|];
    [|'O'; '.'; 'O'; '.'|];
  |] in

  let new_grid = rotate grid in
  print_grid new_grid
;;

  (*
  dune build try_this.exe && dune exec ./try_this.exe  
  *)
