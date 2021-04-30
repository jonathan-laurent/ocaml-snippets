open Base
open Core_bench

let rec add_assoc ~compare (k, v) = function
  | [] -> [(k, v)]
  | (k', v')::rest ->
    let cmp = compare k k' in
    if cmp > 0 then (k', v') :: add_assoc ~compare (k, v) rest
    else if cmp < 0 then (k, v) :: (k', v') :: rest
    else (k, v) :: rest

let rec add_assoc' ~compare ((k, v) as p) l =
  match l with
  | [] -> [p]
  | ((k', v') as p') :: rest ->
    let cmp = compare k k' in
    if cmp > 0 then p' :: add_assoc' ~compare (k, v) rest
    else if cmp < 0 then p :: l
    else p :: rest

type t = A | B | C

let f = function
  | A -> A
  | B -> C
  | C -> B

let f' = function
  | A as x -> x
  | B -> C
  | C -> B

(* 30% slowdown *)
let profile_add_assoc () =
  let l = List.init 10 ~f:(fun i -> (i, i)) in
  let p = (5, 5) in
  let benchs = [
    Bench.Test.create ~name:"Normal" (fun () -> add_assoc ~compare p l);
    Bench.Test.create ~name:"Optimized" (fun () -> add_assoc' ~compare p l)] in
  benchs |> Bench.make_command |> Core.Command.run

(* No difference *)
let profile_f () =
  let benchs = [
    Bench.Test.create ~name:"Normal" (fun () -> f A);
    Bench.Test.create ~name:"Optimized" (fun () -> f' A)] in
  benchs |> Bench.make_command |> Core.Command.run

let () = profile_f ()