let read_lines file =
  In_channel.with_open_text file In_channel.input_all |> Str.(split (regexp "\n"))
;;

let read_blob file =
  In_channel.with_open_text file In_channel.input_all
;;

(* Function to convert a string to a list of integers *)
let string_to_int_list str =
  (* Splitting the string by one or more spaces *)
  let str_list = Str.split (Str.regexp " +") str in
  (* Converting each substring to an integer *)
  List.map int_of_string str_list
;;

(* pseudo String.ends_with *)
let ends_with input suffix =
  let input_len = String.length input in
  let suffix_len = String.length suffix in
  if input_len < suffix_len then false
  else String.equal (String.sub input (input_len - suffix_len) suffix_len) suffix
;;

(* pseudo String.starts_with *)  
let starts_with input prefix =
  let input_len = String.length input in
  let prefix_len = String.length prefix in
  if input_len < prefix_len then false
  else String.equal (String.sub input 0 prefix_len) prefix