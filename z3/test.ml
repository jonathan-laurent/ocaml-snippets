(* Encountering https://github.com/Z3Prover/z3/issues/2305 on z3.4.8.7 *)
(* Question: how to fix it? Answer: opam install z3.4.8.1 *)

open Z3
open Z3.Arithmetic

let test() =
  let cfg = [("model", "true"); ("proof", "false")] in
	let ctx = (mk_context cfg) in
  let rs = (Real.mk_sort ctx) in
  let x_str = (Symbol.mk_string ctx "x") in
  let x = Expr.mk_const ctx x_str rs in
  let g = Goal.mk_goal ctx true false false in (* models, unsat_cores, proofs *)
  let z = (Real.mk_numeral_i ctx 0) in
  let () = Goal.add g [mk_lt ctx x z] in
  let solver = (Solver.mk_solver ctx None) in
  let () = (List.iter ~f:(fun a -> (Solver.add solver [a])) (Goal.get_formulas g)) in
  let q = Solver.check solver [] in
  let () = Stdio.printf "Solver says: %s\n" (Solver.string_of_status q) in
  match Solver.get_model solver with
  | None -> assert false
  | Some m -> Stdio.printf "Model: \n%s\n" (Model.to_string m)

let () = test()