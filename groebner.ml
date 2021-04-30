
let rec remove_trailing_zeros arr =
  let n = Array.length arr in
  if n > 0 && arr.(n-1) = 0 then remove_trailing_zeros (Array.sub ~pos:0 ~len:(n-1) arr)
  else arr

let rec remove_trailing_zeros arr =
  let len = ref (Array.length arr) in
  while !len > 0 && arr.(!len-1) = 0 do Int.decr len done;
  Array.sub ~pos:0 ~len:!len arr

let () =
  let test i o = [%test_eq: int array] (remove_trailing_zeros i) o in
  test [|0; 1; 0; 1; 0|] [|0; 1; 0; 1|];
  test [|1; 1|] [|1; 1|]

module Exponents = struct
  module T = struct

    type t = int array [@@deriving compare, sexp]
    let create = remove_trailing_zeros
    let var i = assert (i >= 0); Array.init (i+1) ~f:(fun j -> if i = j then 1 else 0)

    let zero = [||]

    let add e e' =
      let n = max (Array.length e) (Array.length e') in
      let s = Array.create ~len:n 0 in
      Array.iteri e  ~f:(fun i x -> s.(i) <- s.(i) + x);
      Array.iteri e' ~f:(fun i x -> s.(i) <- s.(i) + x);
      remove_trailing_zeros s

    (* returns [None] unless e >= e' component-wise *)
    let sub e e' =
      let n = max (Array.length e) (Array.length e') in
      let s = Array.create ~len:n 0 in
      Array.iteri e  ~f:(fun i x -> s.(i) <- s.(i) + x);
      Array.iteri e' ~f:(fun i x -> s.(i) <- s.(i) - x);
      if Array.exists s ~f:(fun e -> e < 0) then None
      else Some (remove_trailing_zeros s)

  end
  include T
  include Comparator.Make(T)
end

(* Constraint: exponents must have the same size everywhere. Not enforced strongly *)

type polynomial = Q.t Map.M(Exponents).t

type monomial = Exponents.t * Q.t

(* Remove monomials with a zero coefficient *)
let normalize = Map.filter ~f:(fun x -> Q.(x <> zero))

let zero = Map.empty (module Exponents)

let var i = Map.singleton (module Exponents) (Exponents.var i) Q.one

let cmul c (p: polynomial) : polynomial =
  if Q.(c = zero) then zero else Map.map p ~f:(fun c' -> Q.(c * c'))

(* Non normalized result *)
let add_unsafe = Map.merge_skewed ~combine:(fun ~key c c' -> Q.add c c')

let add p p' = add_unsafe p p' |> normalize

let add_list ps = List.fold ~init:zero ~f:add_unsafe ps |> normalize

let print = Stdio.print_endline
let fmt = Printf.sprintf

let to_string ~vname p =
  let terms =
    Map.fold p ~init:[] ~f:(fun ~key:exp' ~data:c' mstrs ->
      (* can be "" or "xy^2z^3" *)
      let vars =
        Array.mapi exp' ~f:(fun i e ->
          if e = 0 then None
          else if e = 1 then Some (vname i)
          else Some (vname i ^ "^" ^ (Int.to_string e)))
        |> Array.to_list
        |> List.filter_opt
        |> String.concat ~sep:"" in
      let mstr =
        if Q.(c' = one) then vars else Q.to_string c' ^ "*" ^ vars in
      let mstr = if String.is_empty mstr then "1" else mstr in
      mstr :: mstrs) in
  if List.is_empty terms then "0"
  else String.concat ~sep:" + " terms

let vname = function 0 -> "x" | 1 -> "y" | 2 -> "z" | n -> "u_" ^ (Int.to_string n)

let mul_mono p (exp, c) = Map.fold p ~init:zero ~f:(fun ~key:exp' ~data:c' acc ->
  match Map.add acc ~key:(Exponents.add exp exp') ~data:(Q.mul c c') with
  | `Ok p -> p
  | `Duplicate ->
    print (fmt "p = %s" (to_string ~vname p));
    print (fmt "acc = %s" (to_string ~vname acc));
    print (fmt "exp = %s" ([%show: int array] exp));
    print (fmt "exp' = %s" ([%show: int array] exp'));
    assert false)

(* More efficient when the left one is big *)
let mul p p' = Map.fold p' ~init:zero ~f:(fun ~key:exp' ~data:c' acc ->
  add_unsafe acc (mul_mono p (exp', c')))
  |> normalize

let (+): polynomial -> polynomial -> polynomial = add
let ( * ) = mul
let rat r = Map.singleton (module Exponents) [||] r
let int i = rat (Q.of_int i)
let (~-) p = cmul (Q.of_int (-1)) p
let (-) p p' = p + (-p')
let x: polynomial = var 0
let y: polynomial = var 1
let z: polynomial = var 2

(* Try reduce polynomial f with polynomial g *)
(* let try_reduce f g =
  match  *)

(* Returns [None] if given a zero polynomial *)
let leading_monomial (p: polynomial) = Map.min_elt p

let divide_monomials (exps, c) (exps', c') =
  match Exponents.sub exps exps' with
  | Some m -> assert Q.(c' <> zero); Some (m, Q.div c c')
  | None -> None

(* Reduce the lead monomial of f using the lead monomial of g.
 * One must have lm(g) | lm(f) *)
let lead_reduce f g =
  let open Option.Let_syntax in
  let%bind lmf = leading_monomial f in
  let%bind lmg = leading_monomial g in
  let%bind m = divide_monomials lmf lmg in
  let red = f - mul_mono g m in
  (* TODO: check that red < f *)
  return red

let () =
  (int 1 - x) * (int 1 + y)
  |> to_string ~vname
  |> Stdio.print_endline