open Core 

module NumberCoord = struct
  type t = {
    number: int;
    x1: int;
    x2: int;
    y: int;
    used: bool;
  }

  let empty = {
    number = -1;  (* Sentinel value representing 'empty' *)
    x1 = 0;
    x2 = 0;
    y = 0;
    used = false;
  }

  let copywith ?(number=None) ?(x1=None) ?(x2=None) ?(y=None) ?(used=None) nc =
    {
      number = Option.value number ~default:nc.number;
      x1 = Option.value x1 ~default:nc.x1;
      x2 = Option.value x2 ~default:nc.x2;
      y = Option.value y ~default:nc.y;
      used = Option.value used ~default:nc.used;
    }
  let is_empty nc = 
    nc.number = -1  (* Check if the NumberCoord is 'empty' *)
end

(* let string_of_number_coord nc =
  Printf.sprintf "#: %d, x1: %d, x2: %d, y: %d, used: %B"
    nc.NumberCoord.number nc.NumberCoord.x1 nc.NumberCoord.x2 nc.NumberCoord.y nc.NumberCoord.used
  ;; *)

module SymbolCoord = struct
  type t = {
    x: int;
    y: int;
  }

end

(* let string_of_symbol_coord sc =
  Printf.sprintf "symbol: %c, x: %d, y: %d"
    sc.SymbolCoord.symbol sc.SymbolCoord.x sc.SymbolCoord.y
  ;; *)

(* let symbol_list = ['#'; '$'; '%'; '&'; '*'; '+'; '-'; '/'; '='; '@'] *)
let symbol_list = ['*';]
let is_symbol c = List.mem symbol_list c ~equal:Char.equal

(* let retrieve_local_num_coords num_list y =
  List.filter ~f:(fun nc ->
    (not nc.NumberCoord.used) && (abs (nc.NumberCoord.y - y) <= 1)
  ) num_list
  ;; *)

let retrieve_local_sym_coords sym_list y =
  List.filter ~f:(fun sc ->
    (abs (sc.SymbolCoord.y - y) <= 1)
  ) sym_list
  ;;

(* let sum_and_filter_adjacent x y num_list =
  let rec sum_and_filter_adjacent' x y num_list acc = 
    match num_list with
    | [] -> 
      (acc, [])
    | hd :: tl when (x >= (hd.NumberCoord.x1 - 1)) && (x <= (hd.NumberCoord.x2 + 1)) && (abs (hd.NumberCoord.y - y) <= 1) -> 
      printf "adj Num: %d\n" hd.NumberCoord.number;
      let new_acc = acc + hd.NumberCoord.number in
      sum_and_filter_adjacent' x y tl new_acc
    | hd :: tl -> 
      let (new_acc, new_num_list) = sum_and_filter_adjacent' x y tl acc in
      (new_acc, hd :: new_num_list)
  in
  sum_and_filter_adjacent' x y num_list 0
  ;; *)

let sum_and_filter_adjacent x y num_list =
  let is_adjacent hd =
    (x >= (hd.NumberCoord.x1 - 1)) && (x <= (hd.NumberCoord.x2 + 1)) && (abs (hd.NumberCoord.y - y) <= 1)
  in

  let rec count_adjacent num_list count =
    match num_list with
    | [] -> count
    | hd :: tl when is_adjacent hd -> count_adjacent tl (count + 1)
    | _ :: tl -> count_adjacent tl count
  in

  let adjacent_count = count_adjacent num_list 0 in

  let rec sum_and_filter num_list acc result_list =
    match num_list with
    | [] -> (acc, result_list)
    | hd :: tl when adjacent_count = 2 && is_adjacent hd ->
      printf "adj Num: %d\n" hd.NumberCoord.number;
      sum_and_filter tl (acc * hd.NumberCoord.number) result_list
    | hd :: tl -> sum_and_filter tl acc (hd :: result_list)
  in

  if adjacent_count = 2 then
    sum_and_filter num_list 1 []
  else
    (0, num_list)  (* Return original list unchanged if not exactly 2 adjacent NumberCoord items *)
;;

let get_numbers_on_line line_chars num_list y = 
(* printf "outer  Number Count: %d\n" (List.length num_list); *)
let rec get_numbers_on_line' line_chars num_list x y running_number = 
  (* printf "inner rec Number Count: %d\n" (List.length num_list); *)
  match line_chars with
  (* We have to add the running number *)
  | [] when not (NumberCoord.is_empty running_number) -> 
    let new_running_number = (NumberCoord.copywith ~x2:(Some x) running_number) in
    let new_num_list = new_running_number :: num_list in
    new_num_list
  (* We're done and there is no running number *)  
  | [] -> 
    num_list
  (* Start a new number *)
  | Some hd :: tl when Char.is_digit hd && (NumberCoord.is_empty running_number) -> 
    let new_running_number = { NumberCoord.number = Char.get_digit_exn hd; x1 = x; x2 = x; y = y; used = false } in
    get_numbers_on_line' tl num_list (x + 1) y new_running_number
  (* Continue the number *)  
  | Some hd :: tl when Char.is_digit hd && not (NumberCoord.is_empty running_number) -> 
    let new_number = Int.of_string ((string_of_int running_number.number) ^ (String.of_char hd)) in
    let new_running_number = (NumberCoord.copywith ~number:(Some new_number) ~x2:(Some x) running_number) in
    get_numbers_on_line' tl num_list (x + 1) y new_running_number
  (* We're done with the number *)
  | Some _ :: tl when not (NumberCoord.is_empty running_number) -> 
    (* printf "done with number %s\n" (running_number.number |> Int.to_string); *)
    let new_num_list = running_number :: num_list in
    get_numbers_on_line' tl new_num_list (x + 1) y NumberCoord.empty
  (* We're done with the number and there is no running number *)
  | Some _ :: tl -> 
    get_numbers_on_line' tl num_list (x + 1) y NumberCoord.empty
  (* We're done with the number and there is no running number *)
  | None :: tl ->
    printf "only tail\n";
    get_numbers_on_line' tl num_list (x + 1) y running_number
  in
  get_numbers_on_line' line_chars num_list 1 y NumberCoord.empty
  ;;

let get_symbols_on_line line_chars sym_list y =
  (* printf "outer  Symbol Count: %d\n" (List.length sym_list); *)
  let rec get_symbols_on_line' line_chars sym_list x y = 
    (* printf "inner rec Symbol Count: %d\n" (List.length sym_list); *)
    match line_chars with
    (* We're done with line *)
    | [] -> 
      sym_list
    (* Found a symbol *)
    | Some hd :: tl when is_symbol hd -> 
      let new_symbol = { SymbolCoord.x = x; y = y } in
      (* printf "new_symbol: %c\t(%d,%d)\n" hd new_symbol.x new_symbol.y;  *)
      let new_sym_list = new_symbol :: sym_list in
      get_symbols_on_line' tl new_sym_list (x + 1) y
    | Some _ :: tl -> 
      get_symbols_on_line' tl sym_list (x + 1) y
    | None :: tl ->
      get_symbols_on_line' tl sym_list (x + 1) y
    in
  get_symbols_on_line' line_chars sym_list 1 y
  ;;

let rec get_adjacent_symbols acc local_symbols num_list = 
  match local_symbols with
  | [] -> 
    (acc, num_list)
  | hd :: tl ->
    let (sum, new_num_list) = sum_and_filter_adjacent hd.SymbolCoord.x hd.SymbolCoord.y num_list in
    printf "sum: %d\n" sum;
    (* let (new_acc, new_num_list) = get_adjacent_symbols acc tl new_num_list in
    (new_acc, new_num_list) *)
    get_adjacent_symbols (acc+sum) tl new_num_list
  ;;

let rec sum_schematics acc num_list symbol_list row = 
  printf "row: %d\tacc: %d\n" row acc;
  match In_channel.input_line In_channel.stdin with
  | None 
  | Some "" -> 
    acc
  | Some line -> 
    printf "\nline: %s\n" line;
    let new_symbol_list = get_symbols_on_line (List.map ~f:Option.some (String.to_list line)) symbol_list row in 
    let local_syms = retrieve_local_sym_coords new_symbol_list row in 
    let new_num_list = get_numbers_on_line (List.map ~f:Option.some (String.to_list line)) num_list row in
    printf "acc=> %d\n" acc;
    let (new_acc, new_num_list) = get_adjacent_symbols acc local_syms new_num_list in
    Printf.printf "new_acc: %d\n" new_acc;
    sum_schematics new_acc new_num_list new_symbol_list (row + 1)
  ;;

let () = 
  let numbers : NumberCoord.t list = [] in
  let symbols : SymbolCoord.t list = [] in
  match sum_schematics 0 numbers symbols 1 with
  | 0 -> printf "Whiskey Tango Foxtrot\n"
  | schematics -> printf "Day 3.2: %d\n" schematics

  (*
  dune build puzzle_03_2.exe && cat ../data/day-03-test | dune exec ./puzzle_03_2.exe  
  *)