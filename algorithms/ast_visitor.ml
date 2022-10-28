(* Benchmarking the visitor pattern for traversing ASTs. *)

type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr

class ['a] visitor =
  object (self)
    method private const (x: int) (acc: 'a) = acc
    method private add (acc: 'a) = acc
    method private mul (acc: 'a) = acc
    method expr expr (acc: 'a) =
      match expr with
      | Const x -> acc |> self#const x
      | Add (lhs, rhs) ->
        acc |> self#expr lhs |> self#expr rhs |> self#add
      | Mul (lhs, rhs) ->
        acc |> self#expr lhs |> self#expr rhs |> self#mul
  end

let num_const_visitor e =
  let visitor =
    object
      inherit [int] visitor
      method private const _ x = x + 1
    end in
  visitor#expr e 0

let rec num_const = function
  | Const x -> 1
  | Add (lhs, rhs) | Mul (lhs, rhs) -> num_const lhs + num_const rhs

open Core_bench

let () =
  let e = Add (Mul (Const 1, Const 2), Add (Const 4, Add (Const 5, Const 6))) in
  let benchs = [
    Bench.Test.create ~name:"Standatd" (fun () -> num_const e);
    Bench.Test.create ~name:"Visitor" (fun () -> num_const_visitor e)
  ] in
  benchs |> Bench.make_command |> Command_unix.run