(* Encountering https://github.com/Z3Prover/z3/issues/2305 on z3.4.8.7 *)
(* Question: how to fix it? Answer: opam install z3.4.8.1 *)
(* API documentation: https://z3prover.github.io/api/html/ml/Z3.html *)
(* C++ API: https://z3prover.github.io/api/html/namespacez3.html *)

open Z3
open Z3.Arithmetic

let test () =
  let cfg = [("model", "true"); ("proof", "false")] in
	let ctx = mk_context cfg in
  let rs = Real.mk_sort ctx in
  let x_str = Symbol.mk_string ctx "x" in
  let x = Expr.mk_const ctx x_str rs in
  let z = Real.mk_numeral_i ctx 0 in
  let solver = Solver.mk_simple_solver ctx in
  let () = Solver.add solver [mk_lt ctx x z] in
  let q = Solver.check solver [] in
  let () = Stdio.printf "Solver says: %s\n" (Solver.string_of_status q) in
  match Solver.get_model solver with
  | None -> assert false
  | Some m -> Stdio.printf "Model: \n%s\n" (Model.to_string m)

let is_unsat = function
  | Solver.UNSATISFIABLE -> true
  | _ -> false

let bench () =
  let ctx = mk_context [("model", "false"); ("proof", "false")] in
  let real = Real.mk_sort ctx in
  let x = Expr.mk_const ctx (Symbol.mk_string ctx "x") real in
  let zero = Real.mk_numeral_i ctx 0 in
  let one = Real.mk_numeral_i ctx 1 in
  let fml = Boolean.mk_implies ctx (mk_gt ctx x one) (mk_gt ctx x zero) in
  let fml_neg = Boolean.mk_not ctx fml in
  let solver = Solver.mk_simple_solver ctx in
  (* let solver = Solver.mk_solver_t ctx (Tactic.mk_tactic ctx "smt") in *)
  let check () = begin
    assert (Solver.get_num_assertions solver = 0);
    assert (is_unsat (Solver.check solver [fml_neg]));
  end in
  let check_reset () = begin
    Solver.add solver [fml_neg];
    assert (is_unsat (Solver.check solver []));
    Solver.reset solver;
  end in
  let check_push_pop () = begin
    Solver.push solver;
    Solver.add solver [fml_neg];
    assert (is_unsat (Solver.check solver []));
    Solver.pop solver 1;
  end in
  let open Core_bench in
  let benchs = [
    Bench.Test.create ~name:"Check" check;
    Bench.Test.create ~name:"Reset" check_reset;
    Bench.Test.create ~name:"Push-pop" check_push_pop
  ] in
  benchs |> Bench.make_command |> Core.Command.run

let () = bench ()