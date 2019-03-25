open Linspell

let populate_db db0 filename =
  let in_ch = open_in filename in
  let add_word = add db0 in
  let rec aux db =
    try
      let word = input_line in_ch in
        aux (add_word word)
    with End_of_file ->
      close_in in_ch;
      db
  in
    aux db0

let print_results results =
  List.iter (fun (str, d) -> Printf.printf "%s (%d)\n" str d) results;
  ()

let () =
  let mydb = populate_db (create 2) "words.txt" in
  let my_search = search mydb in
    print_results (my_search "cornafobia");
