open Stdio

let opponent_shape = function
  | "A" -> "Rock"
  | "B" -> "Paper"
  | "C" -> "Scissors"
  | _ -> failwith "Invalid shape"
;;

let hero_shape opp wld = 
  match opp, wld with
  | _, "Y" -> opp
  | "Rock", "X" -> "Scissors"
  | "Rock", "Z" -> "Paper"
  | "Paper", "X" -> "Rock"
  | "Paper", "Z" -> "Scissors"
  | "Scissors", "X" -> "Paper"
  | "Scissors", "Z" -> "Rock"
  | _ -> failwith "Invalid shape"
;;

let shape_score = function
  | "Rock" -> 1
  | "Paper" -> 2
  | "Scissors" -> 3
  | _ -> failwith "Invalid shape"
;;

let outcome_score h o = 
  match h, o with
  | "Rock", "Scissors" 
  | "Scissors", "Paper" 
  | "Paper", "Rock" -> 6
  | _ when h = o -> 3
  | _ -> 0
;;

(* let outcome_description h o = 
  match h, o with
  | "Rock", "Scissors" -> "Rock breaks Scissors"
  | "Scissors", "Paper" -> "Scissors cut Paper"
  | "Paper", "Rock" -> "Paper covers Rock"
  | "Rock", "Paper" -> "Rock covered by Paper"
  | "Scissors", "Rock" -> "Scissors broken by Rock"
  | "Paper", "Scissors" -> "Paper cut by Scissors"
  | _ -> "Draw" *)

let rec score_me running_score =
  match In_channel.input_line In_channel.stdin with
  | None -> running_score
  | Some line ->
      (* printf "Line: %s Score: %d\n" line running_score; *)
      let opponent = opponent_shape (String.sub line 0 1) in
      let hero = hero_shape opponent (String.sub line 2 1) in
      (* printf "%s vs %s: %s s: %d, o: %d so: %d\n" hero opponent (outcome_description hero opponent) (shape_score hero) (outcome_score hero opponent) ((shape_score hero) + (outcome_score hero opponent)); *)
      let running_score = running_score + ((shape_score hero) + (outcome_score hero opponent)) in
      (* printf "Running Score: %d\n" running_score; *)
      score_me running_score;
  ;;

let () = Format.sprintf "RPS Score: %d" (score_me 0) |> print_endline


  (*
  dune build puzzle_2_2.exe && cat ../data/day-2 | dune exec ./puzzle_2_2.exe   
  *)
