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
;;

let trim str =
  let is_space = function
    | ' ' | '\n' | '\r' | '\t' -> true
    | _ -> false
  in
  let len = String.length str in
  let rec find_first_non_space i =
    if i >= len then None
    else if is_space str.[i] then find_first_non_space (i + 1)
    else Some i
  in
  let rec find_last_non_space i =
    if i < 0 then None
    else if is_space str.[i] then find_last_non_space (i - 1)
    else Some i
  in
  match find_first_non_space 0, find_last_non_space (len - 1) with
  | Some i, Some j -> String.sub str i (j - i + 1)
  | _ -> ""  
;;

let split_string str = 
  Str.split (Str.regexp " +") str
;;

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module MinHeap (Ord: ORDERED) : sig
  type t
  val empty : t
  val is_empty : t -> bool
  val insert : t -> Ord.t -> t
  val find_min : t -> Ord.t option
  val delete_min : t -> t option
end = struct
  type t = Empty | Node of Ord.t * t * t

  let empty = Empty

  let is_empty h = h = Empty

  let rec merge h1 h2 = 
    match (h1, h2) with
    | (Empty, _) -> h2
    | (_, Empty) -> h1
    | (Node (x, a1, b1), Node (y, a2, b2)) ->
        if Ord.compare x y <= 0
        then Node (x, b1, merge a1 h2)
        else Node (y, b2, merge h1 a2)

  let insert h x = merge h (Node (x, Empty, Empty))

  let find_min = function
    | Empty -> None
    | Node (x, _, _) -> Some x

  let delete_min = function
    | Empty -> None
    | Node (_, a, b) -> Some (merge a b)
end
