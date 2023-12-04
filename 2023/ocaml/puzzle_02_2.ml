open Core

let parse_line line =
  match String.split line ~on:':' with
  | [game_str; colors_str] ->
    let stripped_game_str = String.strip game_str in
    let game_number_str_option = String.chop_prefix stripped_game_str ~prefix:"Game " in
    (match game_number_str_option with
    | Some game_number_str -> 
        let game_number = Int.of_string game_number_str in
        (game_number, colors_str)
    | None -> failwith "Invalid game number format")
  | _ -> failwith "Invalid line format"

let rec parse_color_segments segments max_red max_blue max_green =
  match segments with
  | [] -> (max_red, max_blue, max_green)
  | segment :: rest ->
    let color_numbers = String.split segment ~on:',' |> List.map ~f:String.strip in
    let (new_max_red, new_max_blue, new_max_green) = 
      List.fold color_numbers ~init:(max_red, max_blue, max_green) ~f:(fun (max_r, max_b, max_g) color_number ->
        match String.split color_number ~on:' ' with
        | [number_str; color] ->
          let number = Int.of_string number_str in
          begin
            match color with
            | "red" -> (Int.max max_r number, max_b, max_g)
            | "blue" -> (max_r, Int.max max_b number, max_g)
            | "green" -> (max_r, max_b, Int.max max_g number)
            | _ -> (max_r, max_b, max_g)  (* Ignore unknown colors *)
          end
        | _ -> (max_r, max_b, max_g))  (* Ignore malformed pairs *)
    in
    parse_color_segments rest new_max_red new_max_blue new_max_green

let rec sum_valid_games accum = 
  match In_channel.input_line In_channel.stdin with
  | Some "" -> accum (* End of games *)
  | Some line -> 
    let (_, colors_str) = parse_line line in
    (* printf "Line: %s\n" line; *)
    (* printf "Game: %d\n" game_number;
    printf "Colors: %s\n" colors_str; *)
    let segments = String.split colors_str ~on:';' |> List.map ~f:String.strip in
    let (max_red, max_blue, max_green) = parse_color_segments segments 0 0 0 in
    (* printf "Max red: %d\n" max_red;
    printf "Max blue: %d\n" max_blue;
    printf "Max green: %d\n" max_green; *)
    let powers = max_red * max_blue * max_green in
    sum_valid_games (accum + powers)
  | None -> accum

let () = 
  match sum_valid_games 0 with
  | 0 -> printf "Whiskey Tang Foxtrot\n"
  | calibrations -> printf "Day 2.2: %d\n" calibrations

  (*
  dune build puzzle_02_2.exe && cat ../data/day-02-test | dune exec ./puzzle_02_2.exe  
  *)