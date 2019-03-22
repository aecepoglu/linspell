open Distance;;

type bk_tree = Node of (int * char list * bk_tree list)

let explode_chars str =
  let len = String.length str in
  let rec aux i acc =
    if i < len then
      aux (i + 1) (str.[i] :: acc)
    else
      List.rev acc
  in
    aux 0 []


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
    add_to_root (explode_chars word) roots

let search tolerance word (roots:bk_tree list) :((int*string) list) =
  let chars_0 = explode_chars word in
  let rec aux d_parent queue = function
    | Node(d, chars, subtrees) :: tl ->
       let d = distance chars_0 chars in
         aux d ((d_parent, tl) :: queue) subtrees
    | [] -> (
        match queue with
        | (d, subtrees) :: q_tl -> aux d q_tl subtrees
        | [] -> []
      )
  in
    aux 0 [] roots

let mytree = []
             |> add "ahmet"
             |> add "mehmet"
             |> add "ali"
             |> add "veli"
             |> add "melih"
             |> add "mehmet"
             |> add "metin"
