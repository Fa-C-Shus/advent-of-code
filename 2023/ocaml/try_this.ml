open Core

let read_grids filename =
  let data = In_channel.read_all filename in
  List.map ~f:(fun grid -> 
    List.map ~f:String.to_list (String.split grid ~on:'\n')) (Str.split (Str.regexp "\n\n") data)

let calc_badness (grid: char list list) part2 axis =
  let rows = List.length grid in
  let cols = List.hd_exn grid |> List.length in
  let calc_axis_badness pos delta_pos get_other_pos =
    let badness = ref 0 in
    for delta = 0 to delta_pos - 1 do
      let pos1 = pos - delta in
      let pos2 = pos + 1 + delta in
      if pos1 >= 0 && pos2 < delta_pos then
        for other = 0 to get_other_pos - 1 do
          let char1, char2 = match axis with
            | `Vertical -> 
              let row = List.nth_exn grid other in
              (List.nth_exn row pos1, List.nth_exn row pos2)
            | `Horizontal -> 
              let row1 = List.nth_exn grid pos1 in
              let row2 = List.nth_exn grid pos2 in
              (List.nth_exn row1 other, List.nth_exn row2 other)
          in
          if (not (Char.equal char1 char2)) then incr badness
        done
    done;
    !badness
  in
  let total = ref 0 in
  for i = 0 to (match axis with `Vertical -> cols - 1 | `Horizontal -> rows - 1) - 1 do
    let badness = 
      match axis with
      | `Vertical -> calc_axis_badness i cols rows
      | `Horizontal -> calc_axis_badness i rows cols
    in
    if badness = (if part2 then 1 else 0) then
      total := !total + 
      match axis with 
      | `Horizontal -> 100 * (i + 1)
      | `Vertical -> 1 * (i + 1)
(*         
        (if axis = `Horizontal then 100 else 1) * (i + 1) *)
  done;
  !total

let () =
  let grids = read_grids "../data/day-13" in
  let total_sum = ref 0 in
  List.iter grids ~f:(fun grid ->
    let ans = (calc_badness grid false `Vertical) + (calc_badness grid false `Horizontal) in
    total_sum := !total_sum + ans;
  );
  Printf.printf "13.1: %d\n" !total_sum;

  let total_sum = ref 0 in
  List.iter grids ~f:(fun grid ->
    let ans = (calc_badness grid true `Vertical) + (calc_badness grid true `Horizontal) in
    total_sum := !total_sum + ans;
  );
  Printf.printf "13.2: %d\n" !total_sum;

  (*
  dune build try_this.exe && dune exec ./try_this.exe  
  *)
