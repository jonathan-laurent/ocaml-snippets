type term = Var of string | Add of term * term

and formula = Eq of term * term | And of formula * formula
[@@deriving visitors {variety= "iter"}]

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
