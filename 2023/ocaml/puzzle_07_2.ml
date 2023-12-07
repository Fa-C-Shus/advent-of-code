open Core

module PokerHand = struct
  type t = {
    hand: String.t;
    rank: int; (* 1-7: high card, 1 pair, 2 pair, 3 of a kind, full house, 4 of a kind, 5 of a kind *)
    bet: int;
  }
end

let and_the_Js_are cc =
  match Map.find cc 'J' with
  | Some value -> value
  | None -> 0

let rank_hand hand =
  let char_counts = String.fold hand ~init:(Map.empty (module Char)) ~f:(fun counts char ->
    Map.update counts char ~f:(function
        | None -> 1
        | Some count -> count + 1
    )
  ) in
  printf "  Char counts: %s\n" (Map.to_alist char_counts |> List.map ~f:(fun (k, v) -> sprintf "%c:%d" k v) |> String.concat ~sep:", ");
  let count_list = Map.data char_counts in
  let js = and_the_Js_are char_counts in
  printf "  Js: %d\n" js;
  match count_list with
  (* 5 of a kind 7 *)
  | [5] -> 7
  (* 4 of a kind 6 *)
  | [4; 1] | [1; 4] -> 
    if js = 1 then 7
    else if js = 4 then 7
    else 6
  (* full house 5 *)  
  | [3; 2] | [2; 3] -> 
    if js = 2 then 7
    else if js = 3 then 7
    else 5
  (* 3 of a kind 4 *)  
  | [3; 1; 1] | [1; 3; 1] | [1; 1; 3] -> 
    if js = 3 then 6
    else if js = 1 then 6
    else 4
  (* 2 pair 3 *)
  | [2; 2; 1] | [2; 1; 2] | [1; 2; 2] -> 
    if js = 2 then 6
    else if js = 1 then 5
    else 3
  (* 1 pair 2 *)
  | [2; 1; 1; 1] | [1; 2; 1; 1] | [1; 1; 2; 1] | [1; 1; 1; 2] -> 
    if js = 2 then 4
    else if js = 1 then 4
    else 2
  (* high card 1 *)
  | _ -> 
    printf "  High card\n";
    if js = 1 then 2
    else 1
  ;;

let card_rank = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> -1
  | 'T' -> 10
  | c -> Char.to_int c - Char.to_int '0'
  ;;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []
;;

let rec tie_breaker hand1 hand2 = 
  match hand1, hand2 with
  | [], [] -> 0
  | h1 :: t1, h2 :: t2 -> 
    if (card_rank h1) > (card_rank h2) then 1
    else if (card_rank h1) < (card_rank h2) then -1
    else tie_breaker t1 t2
  | _ -> 0
  ;;

let rec sorted_insert hands hand = 
  match hands with
  | [] -> [hand]
  | h :: t -> 
    if hand.PokerHand.rank < h.PokerHand.rank then hand :: hands
    else if hand.PokerHand.rank > h.PokerHand.rank then h :: sorted_insert t hand
    else 
      let ties = tie_breaker (explode hand.PokerHand.hand) (explode h.PokerHand.hand) in
      if ties < 0 then hand :: hands
      else h :: sorted_insert t hand
  ;;

let display_hands hands = 
  List.iter hands ~f:(fun hand -> 
    printf "Hand cards: %s\trank: %d\tbet:%d\n" hand.PokerHand.hand hand.PokerHand.rank hand.PokerHand.bet;
  );;

let rec score_hands acc index hands = 
  match hands with
  | [] -> acc
  | h :: t -> 
    let winnings = h.PokerHand.bet * index in
    score_hands (acc + winnings) (index + 1) t
  ;;

let rec sum_poker_winnings accum index hands = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> 
    printf "Input -> %d: %s\n" index line;
    let hand = 
      match List.nth (Helper.split_string line) 0 with
      | Some value -> value
      | None -> ""
    in
    let bet = 
      match List.nth (Helper.split_string line) 1 with
      | Some value -> int_of_string value
      | None -> 0
    in
    let rank = rank_hand hand in
    printf "  Hand: %s\n" hand;
    printf "  Bet: %d\n" bet;
    printf "  Rank: %d\n" rank;
    let new_hand = {PokerHand.hand = hand; PokerHand.rank = rank; PokerHand.bet = bet} in
    let new_hands = sorted_insert hands new_hand in
    sum_poker_winnings (accum + 1) (index + 1) new_hands
  | None -> 
    (* End of input *)
    display_hands hands;
    score_hands 0 1 hands
  ;;
  
let () = 
  (* let hand = {PokerHand.hand = "AAAAA"; PokerHand.rank = 5; PokerHand.bet = 10} in
  printf "Hand: %s\n" hand.hand;
  printf "Rank: %d\n" hand.rank;
  printf "Bet: %d\n" hand.bet; *)
  match sum_poker_winnings 0 1 [] with
  | 0 -> printf "No points for you\n"
  | score -> printf "Total Poker Score: %d\n" score

  (*
  dune build puzzle_07_2.exe && cat ../data/day-07-test | dune exec ./puzzle_07_2.exe   
  part 1 answer: 246163188
  failed too big: 246350798 
            maybe 245794069
  *)