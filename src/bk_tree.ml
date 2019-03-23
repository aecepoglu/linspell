open Distance;;

type bk_tree = Node of (int * char list * bk_tree list)

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

let add (word:string) (roots:bk_tree list) :(bk_tree list) =
  let rec add_to_trees chars d = function
    | Node(d', chars', subtrees) :: _ when d' = d -> add_to_trees
                                                       chars
                                                       (distance chars chars')
                                                       subtrees
    | _ :: tl -> add_to_trees chars d tl
    | [] -> [ Node(d, chars, []) ]
  in
  let add_to_root chars = function
    | [] -> [ Node(0, chars, []) ]
    | Node(_, chars', subtrees) :: _ ->
       let d = distance chars chars' in
         add_to_trees chars d subtrees
  in
    add_to_root (chars_of_string word) roots

let search tolerance roots word =
  let chars_0 = chars_of_string word in
  let rec aux d_parent queue results = function
    | Node(d, _, _) :: tl when (abs d - d_parent) <= tolerance ->
       aux d_parent queue results tl
    | Node(_, chars, subtrees) :: tl ->
       let d = distance chars_0 chars in
         aux d ((d_parent, tl) :: queue) ((chars, d) :: results) subtrees
    | [] -> (
        match queue with
        | (d, subtrees) :: q_tl -> aux d q_tl results subtrees
        | [] -> List.map (fun (chars, d) -> string_of_chars chars, d) results
      )
  in
    aux 0 [] [] roots
