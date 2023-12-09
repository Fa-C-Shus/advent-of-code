open Core

module StringMap = Map(String)

let () =
  let my_map = StringMap.empty in
  let my_map = StringMap.set my_map ~key:"key1" ~data:"value1" in
  let my_map = StringMap.set my_map ~key:"key2" ~data:"value2" in

  match StringMap.find my_map "key1" with
  | Some value -> printf "Found: %s\n" value
  | None -> printf "Not found\n"


  (*
  dune build try_this.exe && dune exec ./try_this.exe  
  *)
