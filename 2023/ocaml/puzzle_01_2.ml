open Base 
open Stdio

let rec find_first_digit str =
  match String.length str with
  | 0 -> None  (* No string found *)
  | _ ->
    let ch = String.get str 0 in
    match ch with
    | '0'..'9' -> Some ch  (* Found a digit, return its value *)
    | _ -> find_first_digit (String.sub str ~pos:1 ~len:(String.length str - 1));;  (* Continue searching in the rest of the string *)

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
  find_last_digit' str (String.length str - 1);;

let number_map =
  [ "one", "1"
  ; "two", "2"
  ; "three", "3"
  ; "four", "4"
  ; "five", "5"
  ; "six", "6"
  ; "seven", "7"
  ; "eight", "8"
  ; "nine", "9"
  ; "1", "1"
  ; "2", "2"
  ; "3", "3"
  ; "4", "4"
  ; "5", "5"
  ; "6", "6"
  ; "7", "7"
  ; "8", "8"
  ; "9", "9"];;

let map_to_digit str pos =
  List.find_map number_map ~f:(fun (substr, value) ->
    match String.substr_index ~pos str ~pattern:substr with
    | Some matched when matched = pos -> Some value
    | _ -> None)
;;

let str_to_numbers str =
  let map_to_number = map_to_digit str in
  List.range 0 (String.length str) |> List.filter_map ~f:map_to_number
;;  

let rec sum_calibrations accum = 
  match In_channel.input_line In_channel.stdin with
  | Some "" -> accum (* End of elf *)
  | Some line -> 
    printf "Line: %s\n" line; 
    let fixedline = str_to_numbers line in
    let fixed_line_str = (String.concat fixedline) in
    (* printf "Fixed line: %s\n" fixed_line_str; *)
    let first_digit_char = find_first_digit fixed_line_str in
    let first_digit_str = Char.escaped (Option.value_exn first_digit_char) in
    let last_digit_char = find_last_digit fixed_line_str in
    let last_digit_str = Char.escaped (Option.value_exn last_digit_char) in
    let digits = first_digit_str ^ last_digit_str in
    let calibration_value = Int.of_string digits in
    printf "Calibration value: %d\n" calibration_value;
    sum_calibrations (accum + calibration_value)
  | None -> accum

let () = 
  match sum_calibrations 0 with
  | 0 -> printf "No elves\n"
  | calibrations -> printf "Day 1.1: %d\n" calibrations

  (*
  dune build puzzle_01_2.exe && cat ../data/day-01 | dune exec ./puzzle_01_2.exe  
  *)