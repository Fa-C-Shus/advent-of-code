open Base 
open Stdio

let rec find_first_digit str =
  match String.length str with
  | 0 -> None  (* No string found *)
  | _ ->
    let ch = String.get str 0 in
    match ch with
    | '0'..'9' -> Some ch  (* Found a digit, return its value *)
    | _ -> find_first_digit (String.sub str ~pos:1 ~len:(String.length str - 1))  (* Continue searching in the rest of the string *)

let find_last_digit str =
  let rec find_last_digit' str pos =
    match String.length str with
    | 0 -> None
    | _ ->
      let ch = String.get str pos in
      match ch with
      | '0'..'9' -> Some ch
      | _ -> find_last_digit' str (pos - 1)
  in
  find_last_digit' str (String.length str - 1)

let rec sum_calibrations accum = 
  match In_channel.input_line In_channel.stdin with
  | Some "" -> accum (* End of elf *)
  | Some line -> 
    let first_digit_char = find_first_digit line in
    let first_digit_str = Char.escaped (Option.value_exn first_digit_char) in
    let last_digit_char = find_last_digit line in
    let last_digit_str = Char.escaped (Option.value_exn last_digit_char) in
    let digits = first_digit_str ^ last_digit_str in
    let calibration_value = Int.of_string digits in
    sum_calibrations (accum + calibration_value)
  | None -> accum

let () = 
  match sum_calibrations 0 with
  | 0 -> printf "No elves\n"
  | calibrations -> printf "Day 1.1: %d\n" calibrations

  (*
  dune build puzzle_01_1.exe && cat ../data/day-01 | dune exec ./puzzle_01_1.exe  
  *)