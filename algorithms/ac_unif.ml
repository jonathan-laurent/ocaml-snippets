(* Short implementation of unification modulo AC *)

(* Why unification modulo AC is hard
 * Testing equality is simple: just use normalization
 * Solving unification is much harder (even when one term is concrete).
 * For example, try unifying: f(x + 2) + g(y) + 1 and
 * Another subtelty: find a subexpression that unifies with a pattern
 *     Find `a + c` in `(a + b) + c`?
 *     It is impossible to unify with a subexpression. You would have to rearrange first
 *     or to unify with `a + c + X` where X would match the remaining.
 * Handling units
 *     Example: apply `xyx'=y` on `a*b*(a*b)'` by matching `y` to 1.
 *     Just modify split.
 * Can we match `a + 2` with `a + 1 + X`? No, we would need smart matching here
 * But we can match `a` with `a-b+b`
 * For smart matching, use in conjunction with simplification: introduce term.
 * alternative: make term appear.
 *)

open Base
open Base_quickcheck

type identifier = string
[@@deriving sexp, eq, compare, hash, quickcheck]

type binop = Add | Mul
[@@deriving sexp, eq, compare, hash, quickcheck]

type expr =
  | Const of int
  | Var of identifier
  | MetaVar of identifier
  | Binop of expr * binop * expr
[@@deriving sexp, eq, compare, hash, quickcheck]

type subst = (identifier * expr) list
[@@deriving sexp]

let rec flatten op = function
  | Binop (lhs, op', rhs) when equal_binop op op' ->
    flatten op lhs @ flatten op rhs
  | x -> [x]

let rec combine op = function
  | [] -> failwith "empty"
  | [x] -> x
  | x::xs -> Binop (x, op, combine op xs)

let rec metavars_dup = function
  | Const _ | Var _ -> []
  | MetaVar x -> [x]
  | Binop (lhs, x, rhs) -> metavars_dup lhs @ metavars_dup rhs

(* Returns 2^n possibilities *)
let rec split = function
  | [] -> [([], [])]
  | x::xs ->
    let rest = split xs in
    let xleft = List.map rest ~f:(fun (lhs, rhs) -> (x::lhs, rhs)) in
    let xright = List.map rest ~f:(fun (lhs, rhs) -> (lhs, x::rhs)) in
    xleft @ xright

let rec subst s = function
  | MetaVar x ->
    begin match List.Assoc.find s ~equal:equal_identifier x with
    | None -> MetaVar x
    | Some e -> e
    end
  | Const n -> Const n
  | Var x -> Var x
  | Binop (lhs, op, rhs) -> Binop (subst s lhs, op, subst s rhs)

(* Unify a concrete expression with a pattern *)
let rec unify pat expr =
  let open Option.Let_syntax in
  let fail = None in
  match pat, expr with
  | Const n, Const n' ->
    if n = n' then return [] else fail
  | Const _, _ -> fail
  | Var x, Var x' ->
    if equal_identifier x x' then return [] else fail
  | Var _, _ -> fail
  | MetaVar x, _ ->
    if List.mem (metavars_dup expr) ~equal:equal_identifier x then fail
    else return [(x, expr)]
  | Binop (patl, op, patr), _ ->
      expr |> flatten op |> split
      |> List.filter ~f:(fun (lhs, rhs) ->
        not (List.is_empty lhs || List.is_empty rhs))
      |> List.find_map ~f:(fun (lhs, rhs) ->
        let lhs = combine op lhs in
        let rhs = combine op rhs in
        let%bind s = unify patl lhs in
        let%bind s' = unify (subst s patr) (subst s rhs) in
        return (s @ s'))

let rec normalize e =
  match e with
  | Const _ | Var _ | MetaVar _ -> e
  | Binop (_, op, _) -> e
    |> flatten op
    |> List.map ~f:normalize
    |> List.sort ~compare:compare_expr
    |> combine op

let ac_equal e e' = equal_expr (normalize e) (normalize e')

module ExprBuilder = struct
  let const x = Const x
  let var str = Var str
  let metavar str = MetaVar str
  let ( + ) lhs rhs = Binop (lhs, Add, rhs)
  let ( * ) lhs rhs = Binop (lhs, Mul, rhs)
  let sq x = x * x
end

(* Benchmarking code *)

open Core_bench

let profile () =
  let open ExprBuilder in
  let e = const 1 + sq (var "x") + var "d" + const 2 * var "x"  in
  let pat = sq (metavar "X") + const 2 * metavar "X" + const 1 + metavar "R" in
  let computation () = unify pat e in
  let () = computation ()
  |> [%sexp_of: subst option]
  |> Sexp.to_string_hum
  |> Stdio.print_endline in
  let benchs = [
    Bench.Test.create ~name:"AC Unification" computation;
    Bench.Test.create ~name:"AC equality" (fun () -> ac_equal e e)
  ] in
  benchs |> Bench.make_command |> Command_unix.run

(* Test suite *)

let quickcheck_generator_expr ~vars ~metavars ~consts =
  let open Generator.Let_syntax in
  let vars = List.map vars ~f:(fun x -> Var x) in
  let metavars = List.map metavars ~f:(fun x -> MetaVar x) in
  let consts = List.map consts ~f:(fun n -> Const n) in
  let base_cases = [vars; metavars; consts]
    |> List.filter ~f:(fun l -> not (List.is_empty l))
    |> List.map ~f:Generator.of_list in
  Generator.recursive_union base_cases
    ~f:(fun self -> [
      Generator.map3
        self (Generator.of_weighted_list [(2.0, Add); (1.0, Mul)]) self
        ~f:(fun lhs op rhs -> Binop (lhs, op, rhs))
    ])

let tests () =
  Test.run_exn
  (module struct
    let vars = ["a"; "b"]
    let metavars = ["X"; "Y"]
    let consts = [0; 1]
    type pat = expr
      [@quickcheck.generator quickcheck_generator_expr ~vars ~metavars ~consts]
      [@@deriving sexp, quickcheck]
    type concrete = expr
      [@quickcheck.generator quickcheck_generator_expr ~vars ~metavars:[] ~consts]
      [@@deriving sexp, quickcheck]
    type t = pat * concrete [@@deriving sexp, quickcheck]
  end)
  ~f:(fun (pat, expr) ->
    let unif = unify pat expr in
    unif |> Option.iter ~f:(fun unif ->
      assert (ac_equal (subst unif pat) expr);
      pat  |> [%sexp_of: expr] |> Sexp.to_string_hum |> Stdio.print_endline;
      expr |> [%sexp_of: expr] |> Sexp.to_string_hum |> Stdio.print_endline;
      unif |> [%sexp_of: subst] |> Sexp.to_string_hum |> Stdio.print_endline;
      if List.length unif >= 2 then Stdio.print_endline "Interesting!";
      Stdio.print_endline ""))

let () = tests ()