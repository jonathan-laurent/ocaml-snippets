open Base

type expr =
  | Var of int
  | App of expr * expr
  | Lam of expr
  [@@deriving eq]

(* Convenience constructors *)

let var i = Var i
let app f x = App (f, x)
let lam e = Lam e

let app_list =
  let rec aux acc = function
  | [] -> acc
  | e::es -> aux (App (acc, e)) es in
  function
  | [] -> assert false
  | e::es -> aux e es

(* Pretty printing *)

let parens s = "(" ^ s ^ ")"

let prec = function
  | Var _ -> 3
  | App _ -> 2
  | Lam _ -> 1

let rec to_string e =
  match e with
  | Var x -> Int.to_string x
  | App (f, x) ->
    to_string_paren ~parent:e ~strict:true f ^ " " ^
    to_string_paren ~parent:e ~strict:false x
  | Lam e -> "λ " ^ to_string e

and to_string_paren ~parent ~strict e =
  let s = to_string e in
  let p = prec e in
  let pp = prec parent in
  if pp > p || (pp = p && not strict) then parens s else s

(* Parsing *)

module Parse = struct
  open Angstrom
  let parens p = char '(' *> p <* char ')'
  let integer = take_while1 Char.is_digit >>| Int.of_string
  let expr = fix (fun expr ->
    let atom =
      (integer >>| fun d -> Var d) <|>
      parens expr in
    ((string "λ " *> expr) >>| fun e -> Lam e) <|>
    (sep_by1 (char ' ') atom >>| app_list))
end

let of_string s =
  Angstrom.(parse_string ~consume:All Parse.expr) s
  |> Result.ok_or_failwith

(* Substitution *)

let shift_free d =
  let rec aux nested = function
  | Var x -> Var (if x > nested then x + d else x)
  | Lam e -> Lam (aux (nested + 1) e)
  | App (e, e') -> App (aux nested e, aux nested e') in
  aux 0

let tag_substituted =
  let rec aux n = function
  | Var x -> Var (if x = n then 0 else x)
  | App (e, e') -> App (aux n e, aux n e')
  | Lam e -> Lam (aux (n + 1) e) in
  aux 1

let subst_tags e =
  let rec aux n = function
  | Var 0 -> shift_free n e
  | Var x -> Var x
  | App (e, e') -> App (aux n e, aux n e')
  | Lam e -> Lam (aux (n + 1) e) in
  aux 0

let subst m n = m |> tag_substituted |> shift_free (-1) |> subst_tags n

(* Beta reduction *)

let rec reduction_step = function
  | App (Lam m, n) -> subst m n, true
  | App (e, e') ->
    let e, changed = reduction_step e in
    if changed then App (e, e'), changed
    else
      let e', changed = reduction_step e' in
      App (e, e'), changed
  | Lam e -> let e, changed = reduction_step e in Lam e, changed
  | Var x -> Var x, false

let rec reduce e =
  let e, changed = reduction_step e in
  if changed then reduce e else e

(* Church numerals *)

(* [0] = λf.λx.x  *)
let zero = "λ λ 1" |> of_string

(* [succ] = λn.λf.λx.f(nfx) *)
let succ = "λ λ λ 2 (3 2 1)" |> of_string

let church n =
  let rec aux = function
  | 0 -> zero
  | n -> app succ (aux (n-1)) in
  aux n |> reduce

let of_church = function
  | (Lam (Lam e)) ->
    let rec aux = function
    | Var 1 -> Some 0
    | App (Var 2, e) -> aux e |> Option.bind ~f:(fun n -> Some (n + 1))
    | _ -> None in
    aux e
  | _ -> None

(* [add] = λn.λm.λf.λx.nf(mfx) *)
let add = "λ λ λ λ 4 2 (3 2 1)" |> of_string

(* [mul] = λn.λm.λf.λx.n(mf)x *)
let mul = "λ λ λ λ 4 (3 2) 1" |> of_string

(* [exp] = λn.λm.λf.λx.mnfx *)
let exp = "λ λ λ λ 3 4 2 1" |> of_string

(* Tests *)

let test_subst () =
  let f = "λ 4 2 (λ 1 3)" |> of_string in
  let x = "λ 5 1" |> of_string in
  subst f x |> to_string |> Stdio.print_endline

let test_red () =
  let e = "(λ λ 4 2 (λ 1 3)) (λ 5 1)" |> of_string in
  e |> reduce |> to_string |> Stdio.print_endline

let test_church () =
  let pr e = to_string e |> Stdio.print_endline in
  for i = 0 to 5 do
    assert ([%equal: int option] (Some i) (of_church (church i)));
    pr (church i)
  done

let test_ops () =
  let ex op a b =
    app_list [op; church a; church b] |> reduce |> of_church
    |> [%show: int option] |> Stdio.print_endline in
  ex add 3 4;
  ex mul 6 7;
  ex exp 2 10

let () = test_church (); Stdio.print_endline ""; test_ops ()