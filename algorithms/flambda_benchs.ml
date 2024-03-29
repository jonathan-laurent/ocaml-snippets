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

let enum_map = function
  | A -> A
  | B -> C
  | C -> B

let enum_map' = function
  | A as x -> x
  | B -> C
  | C -> B

type 'a union_shared =
  | Int_s of 'a * int
  | Bool_s of 'a * bool

let annot_s = function
  | Int_s (a, _) -> a
  | Bool_s (a, _) -> a

type 'a union_shared_bad =
  | Int_sb of 'a * int
  | Bool_sb of bool * 'a

let annot_sb = function
  | Int_sb (a, _) -> a
  | Bool_sb (_, a) -> a

type 'a union_non_shared = 'a * int_or_bool
and int_or_bool = Int_ns of int | Bool_ns of bool

let annot_ns = fst

(* Almost twice slower than the version that does not allocate *)
let sum_squares l =
  List.map l ~f:(fun x -> x * x)
  |> List.fold ~init:0 ~f:(+)

let sum_squares' l =
  List.fold l ~init:0 ~f:(fun acc x -> acc + x * x)

let compose_map l =
  List.map l ~f:(fun x -> 2 * x) |> List.map ~f:(fun x -> x + 1)

let compose_map' l = List.map l ~f:(fun x -> 2 * x + 1)

let profile ~normal ~optimized =
  let benchs = [
    Bench.Test.create ~name:"Normal" normal;
    Bench.Test.create ~name:"Optimized" optimized] in
  benchs |> Bench.make_command |> Command_unix.run

(* ~30% slowdown, with or without flambda *)
let profile_add_assoc () =
  let l = List.init 10 ~f:(fun i -> (i, i)) in
  let p = (5, 5) in
  profile
    ~normal:(fun () -> add_assoc ~compare p l)
    ~optimized:(fun () -> add_assoc' ~compare p l)

(* No difference *)
let profile_enum_map () =
  profile
    ~normal:(fun () -> enum_map A)
    ~optimized:(fun () -> enum_map' A)

let profile_sum_squares () =
  let l = [1; 2; 3; 4; 5; 6] in
  profile
    ~normal:(fun () -> sum_squares l)
    ~optimized:(fun () -> sum_squares' l)

let profile_compose_map () =
  let l = [1; 2; 3; 4; 5; 6] in
  profile
    ~normal:(fun () -> compose_map l)
    ~optimized:(fun () -> compose_map' l)

(* Inconclusive *)
let profile_union_shared () =
  (* let normal () = List.map ~f:annot_s [Int_s (0, 1); Bool_s (0, true)] in *)
  let normal () = List.map ~f:annot_sb [Int_sb (0, 1); Bool_sb (true, 0)] in
  let optimized () = List.map ~f:annot_ns [(0, Int_ns 1); (0, Bool_ns true)] in
  profile ~normal ~optimized

let () = profile_union_shared ()