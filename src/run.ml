open Lib

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
  let words =
    "I started my schooling as the majority did in my area, at the local primarry school. I then went  to  the  local  secondarry  school  and  recieved  grades  in  English,  Maths,  Phisics,  Biology, Geography, Art, Graphical Comunication and Philosophy of Religeon. I'll not bore you with the 'A' levels and above. Notice the ambigous English qualification above. It was, in truth, a cource dedicated to reading  \"Lord  of  the  flies\"  and  other  gems,  and  a  weak  atempt  at  getting  us  to  commprehend them. Luckilly my middle-class upbringing gave me a head start as I was already aquainted with that sort of langauge these books used (and not just the Peter and Jane books) and had read simillar books before. I will never be able to put that paticular course  down  as  much  as  I  desire  to  because,  for  all  its  faults,  it  introduced  me  to  Steinbeck, Malkovich and the wonders of Lenny, mice and pockets. My  education  never  included  one  iota  of  grammar.  Lynn  Truss  points  out  in  \"Eats,  shoots and leaves\" that many people were excused from the rigours of learning English grammar  during  their  schooling  over  the  last  30  or  so  years  because  the  majority  or  decision-makers  decided  one  day  that  it  might  hinder  imagination  and  expresion  (so  what, I ask, happened to all those expresive and imaginative people before the ruling?)."
    |> String.split_on_char ' '
  in
  print_endline "Setting up the DB... Please wait.";
  let mydb = populate_db (create 2) "words.txt" in
    Printf.printf "testing words...\n";
  let t0 = Sys.time () in
    List.iter (fun w ->
        let _ = w
                |> search mydb
                |> print_results in
          ();
      ) words;
    let t1 = Sys.time () in
      Printf.printf "took %f\n" (t1 -. t0);
