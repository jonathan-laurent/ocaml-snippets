type expr = Var of string | Add of expr * expr | Neg of expr

type visitor =
  { mutable visit_var: string -> unit
  ; mutable visit_add: expr -> expr -> unit
  ; mutable visit_neg: expr -> unit
  ; mutable visit_expr: expr -> unit }

let dummy_visitor () =
  { visit_var= (fun _ -> ())
  ; visit_add= (fun _ _ -> ())
  ; visit_neg= (fun _ -> ())
  ; visit_expr= (fun _ -> ()) }

let set_visitor_methods self =
  self.visit_expr <-
    (function
    | Var s ->
        self.visit_var s
    | Add (e1, e2) ->
        self.visit_add e1 e2
    | Neg e ->
        self.visit_neg e ) ;
  self.visit_var <- (fun s -> self.visit_var s) ;
  self.visit_add <- (fun e1 e2 -> self.visit_expr e1 ; self.visit_expr e2) ;
  self.visit_neg <- (fun e -> self.visit_expr e)

let make_visitor f =
  let super = dummy_visitor () in
  let self = dummy_visitor () in
  set_visitor_methods super ; set_visitor_methods self ; f ~super ~self ; self

let vars expr =
  let vars = Queue.create () in
  let visitor =
    make_visitor (fun ~super:_ ~self ->
        self.visit_var <- (fun s -> Queue.add s vars) )
  in
  visitor.visit_expr expr ;
  List.of_seq (Queue.to_seq vars)

let%expect_test "alternate visitor" =
  let expr = Add (Var "x", Neg (Add (Var "y", Var "z"))) in
  let vars = vars expr in
  List.iter print_string vars ;
  [%expect {| xyz |}]
