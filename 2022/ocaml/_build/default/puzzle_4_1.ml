open Stdio

module IntSet = Set.Make(Int)

let rec play accum = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> 
      let elf1 = line |> String.split_on_char ',' |> List.hd in
      (* assumes there are 2 elements in the list *)
      let elf2 = line |> String.split_on_char ',' |> List.tl |> List.hd in
      let elf1Start = String.split_on_char '-' elf1 |> List.hd |> int_of_string in
      let elf1End = String.split_on_char '-' elf1 |> List.tl |> List.hd |> int_of_string in
      let elf2Start = String.split_on_char '-' elf2 |> List.hd |> int_of_string in
      let elf2End = String.split_on_char '-' elf2 |> List.tl |> List.hd |> int_of_string in
      let elf1Schedule = IntSet.of_list( List.init (elf1End - elf1Start + 1) (fun i -> i + elf1Start) )in
      let elf2Schedule = IntSet.of_list( List.init (elf2End - elf2Start + 1) (fun i -> i + elf2Start) )in
     ( match (elf1Schedule, elf2Schedule) with
      | _ when (IntSet.subset elf1Schedule elf2Schedule) -> 
        (* printf "elf1 is repeating elf2 camps: %s\n" line; *)
        play (accum + 1)
      | _ when (IntSet.subset elf2Schedule elf1Schedule) -> 
        (* printf "elf2 is repeating elf1 camps: %s\n" line; *)
        play (accum + 1)
      |_ -> play accum
     )
    
  | None -> accum

let () = match play 0 with 
  | 0 -> printf "no savings found\n"
  | n -> printf "savings opportunities: %d\n" n
;;

  (*
  dune build puzzle_4_1.exe && dune exec ./puzzle_4_1.exe   
  dune build puzzle_4_1.exe && cat ../data/day-4-test | dune exec ./puzzle_4_1.exe   
  dune build puzzle_4_1.exe && cat ../data/day-4 | dune exec ./puzzle_4_1.exe   
  *)