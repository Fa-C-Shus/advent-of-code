open Hashtbl

let create_dict str =
  let htbl = create (String.length str) in  (* Initialize the hash table *)
  String.iter (fun key ->
    add htbl key (1, 4000)
  ) str;
  htbl

(* Example usage *)
let () =
  let dict = create_dict "xmas" in
  iter (fun key (a, b) ->
    Printf.printf "%c: (%d, %d)\n" key a b
  ) dict


  (*
  dune build try_this.exe && cat ../data/day-17-test | dune exec ./try_this.exe  
  dune build try_this.exe && dune exec ./try_this.exe    
  *)