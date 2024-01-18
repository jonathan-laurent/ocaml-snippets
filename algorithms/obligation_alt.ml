(* Small programming problem from Jonathan Hellwig *)


type term = Var of string | Const of int | Add of term * term [@@deriving show]

type comp_op = Eq | Le | Ge [@@deriving show]

type formula =
  | True
  | Not of formula
  | Comp of term * comp_op * term
  | Imply of formula * formula
  | Let of string * term * formula
  | And of formula * formula [@@deriving show]

type instr =
  | Assign of string * term
  | Assert of formula
  | Cond of formula * prog * prog

and prog = instr list

let assuming a f = Imply (a, f)
let subst_var x t f = Let (x, t, f)

let rec preconditions prog post =
  match prog with
  | [] -> [post], []
  | instr :: rest ->
    let opened, closed = preconditions rest post in
    match instr with
    | Assign (x, t) ->
      List.map (subst_var x t) opened, closed
    | Assert f ->
      [f], List.map (assuming f) opened @ closed
    | Cond (c, tb, fb) ->
      List.fold_left (fun (opened, closed) o ->
        let opened_t, closed_t = preconditions tb o in
        let opened_f, closed_f = preconditions fb o in
        let opened =
          List.map (assuming c) opened_t @
          List.map (assuming (Not c)) opened_f
          @ opened in
        let closed = closed_t @ closed_f @ closed in
        opened, closed
      ) ([], closed) opened

let obligations prog =
  let opened, closed = preconditions prog True in
  opened @ closed

let test_prog = [
  Assign ("x", Const 1);
  Assert (Comp (Var "x", Ge, Const 0));
  Cond (
    Comp (Var "x", Ge, Const 2),
    [Assign ("x", Const 3)],
    [Assign ("x", Const 6) ; Assert (Comp (Var "x", Ge, Const 6))]
  );
  Assert (Comp (Var "x", Ge, Const 2));
]

let () =
  obligations test_prog |> List.iter (fun o ->
    Stdio.print_endline ([%show: formula] o))