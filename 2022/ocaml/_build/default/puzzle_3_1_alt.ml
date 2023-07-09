open Stdio

module CharSet = Set.Make(Char)

let to_charset input = String.to_seq input |> CharSet.of_seq
;;

let get_priority ch =
  let ord = Char.code ch in 
  match ord with 
  | _ when ord <= Char.code 'Z' -> ord - Char.code 'A' + 27
  | _ when ord <= Char.code 'z' -> ord - Char.code 'a' + 1
  | _ -> 0
;;

let rec play accum elf1 elf2 elf3 =   
  (* printf "play %d %s %s %s\n%!" accum (Option.value elf1 ~default:"None") (Option.value elf2 ~default:"None") (Option.value elf3 ~default:"None"); *)
  match In_channel.input_line In_channel.stdin with
  | Some line -> 
    ( match (elf1, elf2, elf3) with
      | (None, None, None) -> 
        play accum (Some line) None None
      | (Some _, None, None) -> 
        play accum elf1 (Some line) None
      | (Some e1, Some e2, None) -> 
        let e1s = to_charset (e1) in
        let e2s = to_charset (e2) in
        let e3s = to_charset (line) in
        let badge = CharSet.inter e1s e2s |> CharSet.inter e3s |> CharSet.choose in
        let priority = get_priority badge in
       play (accum + priority) None None None
      | (Some _, Some _, Some _) -> 
        play accum (Some line) None None
      | _ -> 
        printf "Invalid elf combo\n%!";
        failwith "Impossible" )
  | _ -> 
      accum
;;

let () = 
  match play 0 None None None with 
  | 0 -> printf "No solution found\n"
  | ans -> printf "Solution: %d \n" ans
;;

  (*
  dune build puzzle_3_1_alt.exe && dune exec ./puzzle_3_1_alt.exe   
  dune build puzzle_3_1_alt.exe && cat ../data/day-3-test | dune exec ./puzzle_3_1_alt.exe   
  dune build puzzle_3_1_alt.exe && cat ../data/day-3 | dune exec ./puzzle_3_1_alt.exe   
  *)