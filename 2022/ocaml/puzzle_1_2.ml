open Base 
open Stdio
(* find the top 3 elves by calorie weight *)

(* Get calories per elf *)
let rec sum_calories accum = 
  match In_channel.input_line In_channel.stdin with
  | Some "" -> accum (* End of elf *)
  | Some line -> sum_calories (accum + Int.of_string line)
  | None -> accum (* End of input *)

(* Check if the next elf is in the top 3 *)
let check_next_elf x (elf1, elf2, elf3) = 
  match x with
  | _ when x > elf1 -> (x, elf1, elf2)
  | _ when x > elf2 -> (elf1, x, elf2)
  | _ when x > elf3 -> (elf1, elf2, x)
  | _ -> (elf1, elf2, elf3)

let rec find_fat_elves (elf1, elf2, elf3) = 
  match sum_calories 0 with
  | 0 -> (elf1, elf2, elf3)
  | next_elf -> find_fat_elves (check_next_elf next_elf (elf1, elf2, elf3))

let () = 
  let (elf1, elf2, elf3) = find_fat_elves (0, 0, 0) in
  printf "The top 3 elves are: %d, %d, %d\n\tTotal: %d\n" elf1 elf2 elf3 (elf1 + elf2 + elf3)

  (*
  dune build puzzle_1_2.exe && cat ../data/day-1 | dune exec ./puzzle_1_2.exe   
  *)