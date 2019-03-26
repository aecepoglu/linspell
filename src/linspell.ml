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

type cursor = Start of char list
            | Continue of (bool * char list * char list) list

let find_nearest_relatives (cur:cursor) =
  let rec aux has_deleted prefix results next_queue = function
    | (h :: (h' :: _ as t), q) when h = h' -> aux false prefix results next_queue (t, q)
    | (h :: t, queue) when not has_deleted -> aux false
                                                (h :: prefix)
                                                results
                                                next_queue
                                                (t, ((true, prefix, t) :: queue))
    | l, queue -> 
       let results' = (if has_deleted
                       then ((List.rev prefix) @ l) :: results
                       else results
                      ) in
       let next_queue' = (if has_deleted
                          then (false, prefix, l) :: next_queue
                          else next_queue
                         ) in
         (match queue with
          | (del', prefix', l') :: q_t -> aux del' prefix' results' next_queue' (l', q_t)
          | [] -> results', Continue next_queue'
         )
  in
    aux false [] [] [] (match cur with
        | Start chars -> chars, []
        | Continue q -> [], q
      )

let add db word =
  let rec aux dist cursor =
    if dist <= db.tolerance
    then
      let results, cursor' = find_nearest_relatives cursor in
        List.iter (fun chars -> Hashtbl.add db.tbl (chars, dist) word) results;
        aux (dist + 1) cursor'
    else
      db
  in
    aux 1 (Start (chars_of_string word))

let search db word =
  let rec remove_repeats = function
    | h :: (h' :: _ as t) when h = h' -> remove_repeats t
    | h :: t -> h :: remove_repeats t
    | [] -> []
  in
  let rec aux dist reduced_words cursor =
    if dist > 2
    then []
    else
      reduced_words
      |> List.map (fun chars -> Hashtbl.find_all db.tbl (chars, dist))
      |> List.flatten
      |> (function
          | [] ->
             let further_reduced_words, cursor' = find_nearest_relatives cursor in
             aux (1 + dist) (reduced_words @ further_reduced_words) cursor'
          | results -> results
                       |> remove_repeats
                       |> List.map (fun x -> x, dist)
        )
  in
  let chars = chars_of_string word in
    aux 0 [chars] (Start chars)
