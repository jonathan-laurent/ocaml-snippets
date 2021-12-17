open Base

type _ typ =
  | Int: int typ
  | Arrow: 'a typ * 'b typ -> ('a -> 'b) typ

type (_, _) equal = Refl: ('a, 'a) equal

let rec eq_typ: type a b. a typ -> b typ -> (a, b) equal option =
  fun t t' ->
  match t, t' with
  | Int, Int -> Some Refl
  | Arrow (l, r), Arrow (l', r') ->
    begin match eq_typ l l', eq_typ r r' with
    | Some Refl, Some Refl -> Some Refl
    | _ -> None
    end
  | _ -> None

type 'a var = string * 'a typ

type value = Value: 'a * 'a typ -> value

type (_, _) expr =
  | Const: int -> ('r, int) expr
  | Add: ('r, int) expr * ('r, int) expr -> ('r, int) expr
  | Var: 'a var -> ('r, 'a) expr
  | App: ('r, 'a -> 'b) expr * ('r, 'a) expr -> ('r, 'b) expr
  | Lam: 'a var * ('b, 'b) expr -> ('r, 'a -> 'b) expr
  | Reset: ('a, 'a) expr -> ('r, 'a) expr
  | Shift: (('a -> 'r) -> 'r, ('a -> 'r) -> 'r) expr -> ('r, 'a) expr

module Env:
sig
  exception Not_found
  type t
  val empty: t
  val lookup: t -> 'a var -> 'a
  val add: t -> 'a var -> 'a -> t
end =
struct
  exception Not_found
  type t = {lookup: 'a. (string * 'a typ) -> 'a}
  let empty = {lookup = fun _ -> raise Not_found}
  let lookup env (x, typ) = env.lookup (x, typ)
  let add: type a. t -> string * a typ -> a -> t =
    fun env (x, typ) v ->
      let lookup: type a. (string * a typ) -> a =
        fun (x', typ') ->
          match eq_typ typ typ' with
          | Some Refl when equal_string x x' -> v
          | _ -> env.lookup (x', typ') in
      {lookup}
end

let rec interpret: type r v a. (r, a) expr -> Env.t -> (a -> r) -> r =
  fun expr env k ->
  match expr with
  | Const x -> k x
  | Var x ->
    k (Env.lookup env x)
  | Add (e, e') ->
    interpret e env (fun e ->
    interpret e' env (fun e' ->
    k (e + e')))
  | Lam (x, e) ->
    let f v = interpret e (Env.add env x v) Fn.id in k f
  | App (f, x) ->
    interpret f env (fun f ->
    interpret x env (fun x ->
    k (f x)))
  | Reset e -> k (interpret e env Fn.id)
  | Shift e -> interpret e env Fn.id k

module Syntax = struct
  let const x = Const x
  let (+) x y = Add (x, y)
  let var x = Var x
  let (@@) f x = App (f, x)
  let lam x e = Lam (x, e)
  let reset e = Reset e
  let shift e = Shift e
end

let () =
  let open Syntax in
  let x = ("x", Int) in
  let f = ("f", Arrow (Int, Int)) in
  let k = ("k", Arrow (Int, Int)) in
  let prog_higher_order =
    let twice = lam f (lam x (var f @@ (var f @@ var x))) in
    let incr = lam x (var x + const 1) in
    (twice @@ incr) @@ const 0 in
  let prog_exception =
    const 2 + (reset (
      const 1 + (reset (
        lam x (var x + shift (lam k (const 42))) @@
        const 2)))) in
  let prog_shift =
    reset (
      const 1 + shift (lam k (
        (var k @@ const 1) + (var k @@ const 2)))) in
  let ex s prog =
    let v = interpret prog Env.empty Fn.id in
    Stdio.print_endline (s ^ ": " ^ [%show: int] v) in
  ex "higher_order" prog_higher_order;
  ex "exception" prog_exception;
  ex "shift" prog_shift