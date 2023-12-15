open Core

let rec get_sub_hash acc input =
  match String.length input with
  | 0 -> acc
  | _ -> 
    let char = String.get input 0 in
    let acc' = ((acc + (Char.to_int char)) * 17) % 256 in
    (* printf "char: %c, acc: %d, new_acc: %d\n" char acc acc'; *)
    get_sub_hash acc' (String.sub input ~pos:1 ~len:((String.length input) - 1))
;;

let process_cmds cmds =
  (* let boxes = Array.make 256 [] in *)
  let boxes = Array.init 256 ~f:(fun _ -> []) in
  List.iter cmds ~f:(fun cmd ->
    match String.chop_suffix cmd ~suffix:"-" with
    | Some name ->
      let h = get_sub_hash 0 name in
      (* printf "name: %s, hash: %d\n" name h; *)
      boxes.(h) <- List.filter ~f:(fun (n, _) -> ((not (String.equal n name)))) boxes.(h)
    | None ->
      match String.rsplit2 cmd ~on:'=' with
      | Some (name, len_str) ->
        let h = get_sub_hash 0 name in
        let len_ = Int.of_string len_str in
        (* printf "name: %s, hash: %d, len: %d\n" name h len_; *)
        if List.exists ~f:(fun (n, _) -> (String.equal n name)) boxes.(h) then
          boxes.(h) <- List.map ~f:(fun (n, v) -> if (String.equal n name) then (n, len_) else (n, v)) boxes.(h)
        else
          boxes.(h) <- (name, len_) :: boxes.(h)
      | None -> ()
  );
  boxes
;;

(* let display_boxes boxes =
  Array.iteri boxes ~f:(fun i box_item ->
    List.iteri box_item ~f:(fun j (n, v) ->
      printf "i: %d\tj: %d\tn: %s\tv: %d\n" i j n v
    )
  )
;; *)

let calculate_p2 boxes =
  let p2 = ref 0 in
  Array.iteri boxes ~f:(fun i box_item ->
    let rev_box_item = List.rev box_item in
    List.iteri rev_box_item ~f:(fun j (_, v) ->
      (* let () = printf "p2: %d\ti: %d\tj: %d\tn: %s\tv: %d\n" !p2 i j n v in *)
      p2 := !p2 + (i + 1) * (j + 1) * v
    )
  );
  !p2
;;

let rec get_hash acc =
  match In_channel.input_line In_channel.stdin with
  | Some line -> 
    (* Split the line on ',' to create a list of substrings *)
    let cmds = String.split line ~on:',' in
    let boxes = process_cmds cmds in
    (* printf "========\n";
    display_boxes boxes;
    printf "========\n"; *)
    let new_acc = (acc + (calculate_p2 boxes)) in
    (* printf "line: %s, acc: %d, new_acc: %d\n" line acc new_acc; *)
    get_hash new_acc
  | None -> acc
;;

let () = 
  match get_hash 0 with
  | 0 -> printf "Whiskey Tango Firetruck\n"
  | hash -> printf "15.2: %d\n" hash
;;

(*
dune build puzzle_15_2.exe && cat ../data/day-15-test | dune exec ./puzzle_15_2.exe  
*)