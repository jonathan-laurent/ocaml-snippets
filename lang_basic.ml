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

type _ expr =
  | Const: int -> int expr
  | Add: int expr list -> int expr
  | Var: 'a var -> 'a expr
  | App: ('a -> 'b) expr * 'a expr -> 'b expr
  | Lam: 'a var * 'b expr -> ('a -> 'b) expr

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

let rec interpret: type a. Env.t -> a expr -> a =
  fun env expr ->
  match expr with
  | Const x -> x
  | Add xs -> List.map xs ~f:(interpret env) |> List.fold ~init:0 ~f:(+)
  | Var v -> Env.lookup env v
  | App (f, x) -> (interpret env f) (interpret env x)
  | Lam (x, e) -> fun v -> interpret (Env.add env x v) e

let () =
  let x = ("x", Int) in
  let prog = App (Lam (x, Add [Var x; Const 1]), Const 2) in
  interpret Env.empty prog |> [%show: int] |> Stdio.print_endline