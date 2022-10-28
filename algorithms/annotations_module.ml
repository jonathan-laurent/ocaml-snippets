open Base

type expr =
  | Var of string
  | Add of expr * expr

let map_children ~f = function
  | Var x -> Var x
  | Add (x, y) -> Add (f x, f y)

let rec map_recursively ~f e = f (map_children ~f:(map_recursively ~f) e)

(* //////////////////////////////////////////////////////////////////////////////////// *)

module Annotated = struct

  type 'a expr = {annot: 'a; body: 'a body}

  and 'a body =
    | Var of string
    | Add of 'a expr * 'a expr

  (* Recompute bodies and annotations recursively *)
  let rec map_recursively ~f e =
    let body = match e.body with
      | Var x -> Var x
      | Add (x, y) -> Add (map_recursively ~f x, map_recursively ~f y) in
    f e.annot body

  (* Update the annotations of a formula without altering its structure *)
  let rec annotate ~f e =
    let body = match e.body with
      | Var x -> Var x
      | Add (x, y) -> Add (annotate ~f x, annotate ~f y) in
    let annot = f e.annot body in
    {annot; body}

  let map_annots ~f = annotate ~f:(fun a _ -> f a)

end

let rec annotate ~f e =
  let body =
    match e with
    | Var x -> Annotated.Var x
    | Add (x, y) -> Annotated.Add (annotate ~f x, annotate ~f y) in
  let annot = f body in
  Annotated.({annot; body})

let rec map_deannotate ~f e =
  let pre = 
    match e.Annotated.body with
    | Annotated.Var x -> Var x
    | Annotated.Add (x, y) -> Add (map_deannotate ~f x, map_deannotate ~f y) in
  f e.annot pre

let deannotate e = map_deannotate ~f:(fun _ x -> x) e

(* Example of annotating a tree *)
let annotate_value state = annotate ~f:(fun e ->
  let open Annotated in
  match e with
  | Var x -> state x
  | Add (x, y) -> x.annot + y.annot)

let () =
  let state x = 1 in
  let annotated = annotate_value state (Add (Var "x", Var "y")) in
  let annotated = Annotated.map_annots ~f:(fun x -> x = 2) annotated in
  assert Annotated.(annotated.annot)

(* //////////////////////////////////////////////////////////////////////////////////// *)
