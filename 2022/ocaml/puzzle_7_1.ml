open Stdio
(* open Map *)

let data = Helper.read_lines "../data/day-7"

module ListItem = struct
  type t = 
  | File of { 
      name: string;
      size: int;
    }
  | Folder of { name: string; }

  let is_dir = function
    | Folder _ -> true
    | _ -> false
  ;;

  let is_file = function
    | File _ -> true
    | _ -> false
  ;;

  let of_string = function
    | l when String.starts_with ~prefix:"dir" l -> 
      Folder { name = String.sub l 4 (String.length l - 4) |> String.trim }
    | l -> 
      let parts = String.split_on_char ' ' l in
      let size = int_of_string (parts |> List.hd |> String.trim) in
      let name = parts |> List.tl |> List.hd |> String.trim in
      File { name; size }
  ;;
end

module Directory = struct
  type t = {
    name: string;
    parent: t option;
    mutable todo: int;
    mutable size: int;
  }

  let create name parent = { name; parent; todo = 0; size = 0 }
  let parent directory = directory.parent

  let to_string directory = Format.sprintf "Dir(%s %d ?=%d)" directory.name directory.size directory.todo
  ;;

  let print_directories directories =
    List.iter (fun dir -> printf "%s | " (to_string dir)) directories;
    print_newline ()
  ;;

  let rec find_root directory =
    match parent directory with
    | None -> directory
    | Some p -> find_root p
  ;;

  let process_listing directory listing =
    directory.todo <- List.filter ListItem.is_dir listing |> List.length;
    directory.size <- List.filter_map (fun item -> match item with
      | ListItem.File f -> Some f.size
      | _ -> None
    ) listing 
    |> List.fold_left (fun acc size -> acc + size) 0
    (* |> List.fold_left (+) 0 *)
  ;;

  let process_directory ~parent ~leaf =
    parent.todo <- parent.todo - 1;
    parent.size <- parent.size + leaf.size;
    parent
  ;;
end

let get_leaves data =

  let move_to_root _ data directory leaves = data, Directory.find_root directory, leaves in
  let move_to_parent _ data directory leaves = data, Directory.parent directory 
  |> Option.get, leaves in
  let move_to_child cmd data directory leaves = 
    data, Directory.create (String.sub cmd 4 (String.length cmd -4)) (Some directory), leaves 
  in
  let list_folder _ lines directory leaves = 
    (* Yadda *)
    let get_ls_lines lines =
      let rec get_ls_lines' lines acc =
        match lines with 
        | [] -> lines, acc
        | hd :: _ when String.starts_with ~prefix:"$" hd -> lines, acc
        | hd :: tail -> get_ls_lines' tail (ListItem.of_string hd :: acc) 
      in
      get_ls_lines' lines []
    in
    let lines, listing = get_ls_lines lines in
    Directory.process_listing directory listing;
    let is_leaf = List.for_all ListItem.is_file listing in
    (lines, directory, if is_leaf then directory :: leaves else leaves)
  in

  let rake_leaves lines = 
    (* Base.Option.apply *)
    let ( >>| ) = Base.Option.( >>| ) in

    let rec rake_leaves' lines directory leaves =
      let get_nav hd =
        match hd with
        | hd when hd = "$ cd /" -> move_to_root
        | hd when hd = "$ cd .." -> move_to_parent
        | hd when String.starts_with ~prefix:"$ cd " hd -> move_to_child
        | hd when String.starts_with ~prefix:"$ ls" hd -> list_folder
        | hd -> 
          printf "Unknown command %s\n!" hd;
          failwith "Unknown command\n" 
      in
      let execute fn = fn (List.hd lines) (List.tl lines) directory leaves in
      match Base.List.hd lines >>| get_nav >>| execute with
      | Some (lines, directory, leaves) -> rake_leaves' lines directory leaves
      | None -> leaves
    in
    rake_leaves' lines (Directory.create "/" None) []
  in
  rake_leaves data
  ;;

let process_leaves leaves acc =
  if false then Directory.print_directories leaves;

  let rec process_leaves' leaves acc =
    match leaves with 
    | [] -> acc 
    | leaf :: leaves ->
      let leaves = 
        match Directory.parent leaf with
        | None -> leaves 
        | Some parent -> 
          let parent = Directory.process_directory ~parent ~leaf in 
          if parent.todo == 0 && parent.size <= 100_000 then
            parent :: leaves
          else 
            leaves
          in
          let acc = 
            if leaf.size <= 100_000 then
              acc + leaf.size
            else
              acc 
            in
          process_leaves' leaves acc
        in
        process_leaves' leaves acc
;;

let leaves = get_leaves data
let answer = process_leaves leaves 0
let () = printf "Answer: %d\n" answer

(*
dune build puzzle_7_1.exe && dune exec ./puzzle_7_1.exe   
*)  