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
  print_endline "Setting up the DB... Please wait.";
  let mydb = populate_db (create 2) "words.txt" in
    while true do
      print_string "\nEnter a word to spellcheck (Ctrl-D to quit): ";
      read_line ()
      |> String.trim
      |> search mydb
      |> print_results
    done;
