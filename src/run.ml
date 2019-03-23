open Bk_tree

let populate_db db0 filename =
  Printf.printf "populate %s\n" filename;
  let in_ch = open_in filename in
  let rec aux db =
    Printf.printf "tick\n";
    try
      let word = input_line in_ch in
      let () = Printf.printf "\"%s\"\n" word in
      let db' = add word db in
        aux db'
    with End_of_file ->
      Printf.printf "EOF\n";
      close_in in_ch;
      db
  in
    aux db0

let print_results results =
  List.iter (fun (str, d) -> Printf.printf "%s (%d)\n" str d) results;
  ()


let () =
  let word_db = populate_db [] "words.txt" in
  let my_search = search 2 word_db in
    print_results (my_search "boook");
    print_results (my_search "dissapointed");
