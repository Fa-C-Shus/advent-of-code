open Stdio

module CharSet = Set.Make(Char)

let data = Helper.read_lines "../data/day-3"

let to_charset input = String.to_seq input |> CharSet.of_seq
;;

let get_priority ch =
  let ord = Char.code ch in 
  match ord with 
  | _ when ord <= Char.code 'Z' -> ord - Char.code 'A' + 27
  | _ when ord <= Char.code 'z' -> ord - Char.code 'a' + 1
  | _ -> 0
;;

let groups elves =
  let sum_priority elf1 elf2 elf3 =
    let elf1 = to_charset elf1 in
    let elf2 = to_charset elf2 in 
    let elf3 = to_charset elf3 in 
    let shared_char = CharSet.inter elf1 elf2 |> CharSet.inter elf3 |> CharSet.choose in 
    (* printf "%c => %d\n" shared_char (get_priority shared_char); *)
    get_priority shared_char in

    let rec group3 fn elves acc =
      match elves with
      | [] -> acc
      | elf1 :: elf2 :: elf3 :: rest -> 
        (group3 [@tailcall]) fn rest (acc + fn elf1 elf2 elf3)
      | _ -> failwith "Bad grouping" in

      group3 sum_priority elves 0
      ;;
      

let () = 
  match groups data with
  | 0 -> printf "No elvin groups\n"
  | calories -> printf "Priority Score: %d\n" calories

  (*
  dune build puzzle_3_2.exe && cat ../data/day-3 | dune exec ./puzzle_3_2.exe   
  dune build puzzle_3_2.exe && dune exec ./puzzle_3_2.exe   
  *)