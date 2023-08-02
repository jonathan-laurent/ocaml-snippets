type term = Var of string | Add of term * term

and formula = Eq of term * term | And of formula * formula

class virtual ['self] iter : object ('self )
  method visit_Add : 'monomorphic. 'env -> term -> term -> unit

  method visit_And : 'monomorphic. 'env -> formula -> formula -> unit

  method visit_Var : 'monomorphic. 'env -> string -> unit

  method visit_Eq : 'monomorphic. 'env -> term -> term -> unit

  method visit_term : 'monomorphic. 'env -> term -> unit

  method visit_formula : 'monomorphic. 'env -> formula -> unit
end

val vars : formula -> string list
