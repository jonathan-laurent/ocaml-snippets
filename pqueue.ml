type priority = int

(* Non balanced binary heap: all descendants of a node have a greater cost *)
type 'a t = Empty | Node of priority * 'a * 'a t * 'a t

let empty = Empty

(* We do not guarantee that the tree will be balanced but we swap left and right
 * children regularly to make the worst-case behavior less likely. *)
let rec insert queue prio elt =
  match queue with
    Empty -> Node(prio, elt, Empty, Empty)
  | Node(p, e, left, right) ->
      if prio <= p
      then Node(prio, elt, insert right p e, left)
      else Node(p, e, insert right prio elt, left)

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

let singleton prio elt = insert empty prio elt

let rec size = function
  | Empty -> 0
  | Node (_, _, l, r) -> 1 + size l + size r