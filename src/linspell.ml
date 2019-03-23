let chars_of_string str =
  let len = String.length str in
  let rec aux i acc =
    if i < len then
      aux (i + 1) (str.[i] :: acc)
    else
      List.rev acc
  in
    aux 0 []

let string_of_chars chars =
  List.map (String.make 1) chars
  |> String.concat ""

type t = {
  tolerance:int;
  tbl:(char list * int, string) Hashtbl.t
}

let create tolerance = {
  tolerance = tolerance;
  tbl = Hashtbl.create 50000;
}

let find_nearest_relatives ?(queue0=[]) word =
  let rec aux has_deleted prefix queue results next_queue = function
    | h :: t when not has_deleted -> (aux
                                        false
                                        (h :: prefix)
                                        (queue @ [(prefix, t)])
                                        results
                                        next_queue
                                        t
                                     )
    | l -> 
       let results' = (if has_deleted
                       then ((List.rev prefix) @ l) :: results
                       else results
                      )
       in
         (match queue with
          | (prefix', l') :: q_t -> aux true prefix' q_t results' next_queue l'
          | [] -> results'
         )
  in
    aux false [] queue0 [] [] (chars_of_string word)

let () =
  find_nearest_relatives "ali"
  |> List.iter (fun chars -> print_endline (string_of_chars chars))
