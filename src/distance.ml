let min3 a b c = min a (min b c)

(** calculates Levanshtein distance between two strings
    Each character change, addition or removal is considered 1 unit of change
    "ali" -> "uli" is 1 change
    "ali" -> "dali" is 1 change
    "ali" -> "li" is 1 change
*)
let distance chars1 chars2 =
  let rec aux = function
    | (h1 :: t1 as l1), (h2 :: t2 as l2) ->
       if h1 = h2
       then aux(t1, t2)
       else 1 + min3
                  (aux (t1, t2))
                  (aux (l1, t2))
                  (aux (t1, l2))
    | l, [] | [], l -> List.length l
  in
    aux (chars1, chars2)
