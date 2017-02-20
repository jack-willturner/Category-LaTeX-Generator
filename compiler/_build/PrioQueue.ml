module PrioQueue =
    struct
      type priority = int
      type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
      let empty = Empty
      let rec insert queue prio elt visited =
        if List.mem elt visited then
          queue
        else (
        match queue with
          Empty -> Node(prio, elt, Empty, Empty)
        | Node(p, e, left, right) ->
            if prio <= p
            then Node(prio, elt, insert right p e visited, left)
            else Node(p, e, insert right prio elt visited, left))
      exception Queue_is_empty
      let rec remove_top = function
          Empty -> raise Queue_is_empty
        | Node(prio, elt, left, Empty) -> left
        | Node(prio, elt, Empty, right) -> right
        | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
                          (Node(rprio, relt, _, _) as right)) ->
            if lprio <= rprio
            then Node(lprio, lelt, remove_top left, right)
            else Node(rprio, relt, left, remove_top right)
      let extract = function
          Empty -> raise Queue_is_empty
        | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
      let rec to_list = function
        Empty -> []
        | Node(prio,elt,left,right) -> elt :: (to_list left) @ (to_list right)
      let rec contains x = function
        | Empty -> false
        | Node(prio,elt,left,right) ->  elt = x || contains x left || contains x right
(*   let rec remove_duplicates = function
        | Empty -> []
        | Node(prio,elt,left, right) -> if exists elt left || exists elt right then
                                          remove_duplicates (remove_top (Node(prio,elt,left,right)))
                                        else
                                          Node(prio,elt,(remove_duplicates(left)),(remove_duplicates(left,right)))) *)
    end;;
