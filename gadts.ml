(* Some examples using GADTs *)
(* See https://discuss.ocaml.org/t/unexpected-gadt-behavior-when-wrapping-a-type-into-a-module/5707 *)

(* The reason we did not use GADTs in Arithnetic is that they are not spported by ppx_deriving *)

type zeroary = ZeroaryPhantom
type unary = UnaryPhantom
type binary = BinaryPhantom
type nary = NaryPhantom


type ('sz, 'a) tuple =
  | Zeroary: (zeroary, 'a) tuple
  | Unary: 'a -> (unary, 'a) tuple
  | Binary: 'a * 'a -> (binary, 'a) tuple
  | Nary: 'a list -> (nary, 'a) tuple

let equal_tuple:
  type sz sz' a. (a -> a -> bool) -> (sz, a) tuple -> (sz', a) tuple -> bool =
  fun eq t t' ->
  match t, t' with
  | Zeroary, Zeroary -> true
  | Unary x, Unary x' -> eq x x'
  | Binary (x, y), Binary (x', y') -> eq x x' && eq y y'
  | Nary xs, Nary xs' -> List.equal eq xs xs'
  | _ -> false

let map_tuple: type sz a b. f:(a -> b) -> (sz, a) tuple -> (sz, b) tuple =
  fun ~f t ->
  match t with
  | Zeroary -> Zeroary
  | Unary x -> Unary (f x)
  | Binary (x, x') -> Binary (f x, f x')
  | Nary xs -> Nary (List.map ~f xs)

let fold_tuple: type sz a acc.
  init:acc -> f:(acc -> a -> acc) -> (sz, a) tuple -> acc =
  fun ~init ~f tup ->
  match tup with
  | Zeroary -> init
  | Unary x -> f init x
  | Binary (x, x') -> let acc = f init x in f acc x'
  | Nary xs -> List.fold ~init ~f xs

type 'sz op =
  | Const: int -> zeroary op
  | Neg: unary op
  | Plus: binary op

let equal_op: type sz sz'. sz op -> sz' op -> bool =
  fun op op' ->
  match op, op' with
  | Const _, Const _ | Neg, Neg | Plus, Plus -> true
  | _ -> false

type expr =
  | Node: 'sz op * ('sz, expr) tuple -> expr
(* [@@deriving eq] *)

let eval_op: type sz. sz op -> (sz, int) tuple -> int =
  fun op args ->
  match op, args with
    | Const x, _ -> x
    | Neg, Unary x -> -x
    | Plus, Binary (x, y) -> x + y

(* Another way to write the signature *)
let eval_op' (type a) (op: a op) (args: (a, int) tuple) =
  match op, args with
    | Const x, _ -> x
    | Neg, Unary x -> -x
    | Plus, Binary (x, y) -> x + y

let rec eval = function
  | Node (op, args) -> eval_op op (map_tuple eval args)

module Infix = struct
  let const x = Node (Const x, Zeroary)
  let ( + ) x y = Node (Plus, Binary (x, y))
  let ( ~- ) x = Node (Neg, Unary x)
  let ( - ) x y = x + (~- y)
end

let () =
  let open Infix in
  let expr = const 1 + (const 3 - const 4) in
  expr |> eval |> Stdio.printf "%d\n"


(* Related: we need to give concrete definitions for the GADTs to work: *)

type zero = ZeroPhantom
type 'n succ = SuccPhantom of 'n

type ('n, 'a) vec =
  | Nil: (zero, 'a) vec
  | Cons: 'a * ('n, 'a) vec -> ('n succ, 'a) vec

let rec map_vec: type n a b. f:(a -> b) -> (n, a) vec -> (n, b) vec =
  fun ~f v ->
  match v with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map_vec ~f xs)



(*

you need to store arity explicitly: 

Interpreted function: 

Uninterpreted name (arity) monotonicity 

Database of names.

type 'sz 

Uninterpreted (Binary, f)

Tup.Z
Tup.U x
Tup.B (x, y)

Tup.Ar.Z
Tup.Ar.U

Tup.Arity.Z
Tup.Arity.U

type 

interpret 

let interpret_function

*)