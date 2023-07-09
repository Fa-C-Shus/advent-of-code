open Base 
open Stdio

let rec sum_calories accum = 
  match In_channel.input_line In_channel.stdin with
  | Some "" -> accum (* End of elf *)
  | Some line -> sum_calories (accum + Int.of_string line)
  | None -> accum

let rec find_fat_elf max_calories = 
  match sum_calories 0 with
  | 0 -> max_calories
  | calories -> 
    if calories > max_calories then find_fat_elf calories
    else find_fat_elf max_calories


let () = 
  match find_fat_elf 0 with
  | 0 -> printf "No elves\n"
  | calories -> printf "Fat elf: %d\n" calories

  (*
  dune build puzzle_1_1.exe && cat ../data/day-1 | dune exec ./puzzle_1_1.exe   
  *)