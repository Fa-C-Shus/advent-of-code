open Core

let still_valid test_list master_list =
  let rec still_valid' test master idx = 
    (* printf "idx: %d\n" idx;
    printf "test: %s\n" (List.to_string ~f:Int.to_string test);
    printf "master: %s\n" (List.to_string ~f:Int.to_string master); *)
    match test, master with
    | [], _ -> true
    | _, [] -> false
    | _, _ ->
      (match idx with
      | 0 -> 
        let test_element = List.nth_exn test idx in
        let master_element = List.nth_exn master idx in
        (match (test_element < master_element) with
        | true -> 
          (* keep going *)
          still_valid' test master (idx + 1)
        | false -> false)
      | _ when idx > (List.length test) - 1 -> true
      | _ ->
        let test_element = List.nth_exn test idx in
        let prev_test_element = List.nth_exn test (idx - 1) in
        let master_element = List.nth_exn master idx in
        let prev_master_element = List.nth_exn master (idx - 1) in
        (match (test_element < master_element && prev_test_element = prev_master_element) with
        | true -> 
          (* keep going *)
          still_valid' test master (idx + 1)
        | false -> false))
  in
  List.length test_list <= List.length master_list && still_valid' test_list master_list 0
;;

  let master_list = [4;1;1] in
  let first_try = [1;1;2;3] in
  let second_try = [1] in
  let third_try = [2] in
  let fourth_try = [1;1] in

  printf "first_try: %b\n" (still_valid first_try master_list);
  printf "second_try: %b\n" (still_valid second_try master_list);
  printf "third_try: %b\n" (still_valid third_try master_list);
  printf "fourth_try: %b\n" (still_valid fourth_try master_list);


  (*
  dune build try_this.exe && dune exec ./try_this.exe  
  *)
