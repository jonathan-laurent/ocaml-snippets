open Base

type expr =
  | Var of string
  | Sum of expr list
  | Prod of expr list
  [@@deriving show]

let terms = function
  | Sum ts -> ts
  | t -> [t]

let rec expand_prod = function
  | [] -> [[]]
  | f::fs ->
    let expanded = expand_prod fs in
    List.concat_map (terms f) ~f:(fun t ->
      List.map ~f:(fun e -> t::e) expanded)

let expand = function
  | Prod es -> Sum (List.map ~f:(fun x -> Prod x) (expand_prod es))
  | e -> e
    
let expr =
  let x, y, z = Var "x", Var "y", Var "z" in
  Prod [Sum [x; y; z]; Sum [x; y; z]]

let profile () =
  let open Core_bench in
  let benchs =
    [ Bench.Test.create ~name:"Expand" (fun () -> expand expr) ] in
  benchs |> Bench.make_command |> Core.Command.run

let () = Stdio.print_endline ([%show: expr] (expand expr))
let () = profile ()