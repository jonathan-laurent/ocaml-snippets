
(* See https://gitlab.inria.fr/fpottier/visitors/-/blob/master/test/test07.ml *)

type term = Var of string | Add of term * term
[@@deriving visitors {variety= "iter"}]

class ['a] iter_term = ['a] iter

type formula = Eq of term * term | And of formula * formula
[@@deriving visitors {variety= "iter"; nude=true; ancestors=["iter_term"]}]

let vars fml =
  let vars = Queue.create () in
  let visitor =
    object
      inherit [_] iter as super

      method! visit_Var env v = Queue.push v vars ; super#visit_Var env v
    end
  in
  visitor#visit_formula () fml ;
  Queue.to_seq vars |> List.of_seq
