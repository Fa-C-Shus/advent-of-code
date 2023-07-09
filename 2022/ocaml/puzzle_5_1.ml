open Stdio

let data = Helper.read_lines "../data/day-5.txt"

module Move = struct
  type t = {
    amount: int;
    src: int;
    dst: int;
  }

  let of_string move =
    let tokens = String.split_on_char ' ' move |> Array.of_list in
    let amount = int_of_string tokens.(1) in
    let src = int_of_string tokens.(3) in
    let dst = int_of_string tokens.(5) in
    { amount; src; dst }
  ;;
end

let print_tops stacks =
  Array.iter (fun stack -> Stack.top stack |> print_char) stacks;
  print_newline ()
;;

let push_row_onto_stacks row stacks =
  (* each column of data occupies 4 spaces *)
  let f_tx_idx idx = (idx * 4) + 1 in (* +1 jumps the open brack [ *)
  let f idx stack =
    match f_tx_idx idx |> String.get row with
    | stk when stk = ' ' -> ()      (* nothing in this column *)
    | stk -> Stack.push stk stack   (* push the letter ont the correct stack *)
  in
  Array.iteri f stacks
;;

let display_stacks stacks =
  let f idx stack =
    printf "%d: " (idx + 1);
    Stack.iter (fun x -> printf "%c" x) stack;
    printf "\n"
  in
  Array.iteri f stacks
;;

let read_board data =
  (* Find the row that starts with a 1 (position 1 <second column>) *)
  let (rows, line) = 
    Base.List.findi ~f:(fun _ line -> String.get line 1 == '1') data 
    |> Option.get in 
  (* There highest stack is 8 high *)
  let x = (Str.last_chars (String.trim line) 1) |> int_of_string in
  printf "We need %d stacks; the highest of which is %d\n" x rows;
  let stacks = Array.init x (fun _ -> Stack.create ()) in
  (* Fill the stacks *)
  for row = rows -1 downto 0 do
    push_row_onto_stacks (List.nth data row) stacks
  done;

  (stacks, rows)
;;

let rec process_moves stacks moves =
  match moves with
  | _ when List.length moves > 0 -> 
    let move = List.hd moves |> Move.of_string in
    let arr  = Array.make move.amount ' ' in
    (* printf "Move %d crates from stack %d to stack %d\n" move.amount move.src move.dst; *)
    for i = 0 to move.amount - 1 do
      arr.(i) <- Stack.pop stacks.(move.src - 1);
      Stack.push arr.(i) stacks.(move.dst - 1);
    done;
    (* display_stacks stacks; *)
    process_moves stacks (List.tl moves)
  | _  -> () (* no more moves *)

;;

let () = 
  let (stacks, rows) = read_board data in
  display_stacks stacks;
  printf "Start instructions at %d\n" (rows + 2);
  let moves = Base.List.drop data (rows + 2) in
  (* List.iter (fun x -> printf "%s\n" x) moves; *)
  process_moves stacks moves;
  (* display_stacks stacks; *)
  print_tops stacks;
;;

  (*
  dune build puzzle_5_1.exe && dune exec ./puzzle_5_1.exe   
  *)