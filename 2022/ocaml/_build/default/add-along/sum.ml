open Base 
open Stdio

let rec read_in_stream accum = 
  match In_channel.input_line In_channel.stdin with
  | None -> accum
  | Some line -> read_in_stream (accum +. Float.of_string line)

let () = 
  printf "Total: %F\n" (read_in_stream 0.0)