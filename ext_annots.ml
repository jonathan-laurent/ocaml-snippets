(** Experiments with external annotations.
    Instead of annotating the formula structure itself, one can
    maintain an annotation tree with the same structure and traverse
    both in parallel. *)

type expr =
  | Const of int
  | Var of string
  | Add of expr list
  [@@deriving show]

type 'a annots_tree = Node of 'a * 'a annots_tree list [@@deriving show]

let annot (Node (x, xs)) = x

(* Memory efficient substitution *)

let rec annotate_with_vars = function
  | Const _ -> Node (Set.empty (module String), [])
  | Var x -> Node (Set.singleton (module String) x, [])
  | Add es ->
    let children = List.map es ~f:annotate_with_vars in
    Node (
      Set.union_list (module String) (List.map ~f:annot children),
      children)

let rec subst' ~var ~substituted (Node (vs, cs)) e =
    if not (Set.mem vs var) then e
    else match e with
    | Const _ -> assert false
    | Var x -> assert (equal_string var x); substituted
    | Add es -> Add (List.map2_exn cs es ~f:(subst' ~var ~substituted))

let subst ~var ~substituted e =
  subst' ~var ~substituted (annotate_with_vars e) e

(* Map recursively *)

let map_children_with_annots e (Node (a, aa)) ~f =
  match e with
  | Const _ | Var _ -> e
  | Add es -> Add (List.map2_exn es aa ~f)

let rec apply_recursively e a ~f =
  f (map_children_with_annots e a ~f:(apply_recursively ~f)) (annot a)