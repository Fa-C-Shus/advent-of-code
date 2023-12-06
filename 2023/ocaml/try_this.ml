open Core

let gather_seeds input =
  let seeds = String.drop_prefix input 7 in
  let seed_list = Helper.string_to_int_list seeds in
  let rec append_range seed_list ret_list =
    match seed_list with
    | start :: len :: rest -> 
      let range = List.range ~start:`inclusive start ~stop:`inclusive (start + len - 1) ~stride:1 in
      printf "start: %d, len: %d, range: %d\n" start len (List.length range);
      append_range rest (List.append ret_list range)
    | _ -> ret_list  
  in
  append_range seed_list []
;;

let input = "seeds: 91926764 235794528 3279509610 325625103 2781720183 218217413 1315129829 102999617 3995609239 143268116 358337926 185836835 1543999077 241888600 1795811745 806228439 2616560939 56204204 869828854 224520829"

let () = 
  let seed_list = gather_seeds input in
  printf "seed_list: %d\n" (List.length seed_list)

  (*
  dune build try_this.exe && dune exec ./try_this.exe  
  *)
