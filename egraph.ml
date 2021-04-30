(* A TOY IMPLEMENTATION OF EGRAPHS
 *
 * Reference:
 *   - Egg website: https://egraphs-good.github.io/
 *   - Egg paper: https://dl.acm.org/doi/pdf/10.1145/3434304
 *   - Zucker Tutorial I: https://www.philipzucker.com/egraph-1/
 *   - Zucker Tutorial II (e-matching): https://www.philipzucker.com/egraph-2/
 *   - Colab: https://colab.research.google.com/drive/1tNOQijJqe5tw-Pk9iqd6HHb2abC5aRid
 *)

(* Union-Find implementation:
 * https://fr.wikipedia.org/wiki/Union-find
 * TODO: we could make identifiers abstract for more safety
   (but then we would need a generative functor so that people cannot mix ids between
   structures) *)

module UnionFind = struct

  let no_parent = -1

  type t = {mutable nodes: node array; mutable next_id: int}
  and node = {parent: int; rank: int}

  let fresh_nodes_table ~len =
    Array.create ~len {parent=no_parent; rank=0}

  let create ~capacity =
    assert (capacity > 0);
    {nodes = fresh_nodes_table ~len:capacity; next_id=0}

  let capacity t = Array.length t.nodes

  let num_elements t = t.next_id

  (* Double the capacity of the union-find *)
  let enlarge t =
    let n = capacity t in
    let nodes = fresh_nodes_table ~len:(2*n) in
    Array.blit ~src:t.nodes ~dst:nodes ~src_pos:0 ~dst_pos:0 ~len:n;
    t.nodes <- nodes

  let fresh_id t =
    let id = t.next_id in
    t.next_id <- t.next_id + 1;
    if id >= capacity t then enlarge t;
    id

  let rec find t i =
    let n = t.nodes.(i) in
    if n.parent = no_parent then i
    else begin
      let root = find t n.parent in
      t.nodes.(i) <- {n with parent=root}; (* path compression *)
      root
    end

  let merge t i j =
    let ri, rj = find t i, find t j in
    if ri <> rj then begin
      let rni, rnj = t.nodes.(ri), t.nodes.(rj) in
      if rni.rank < rnj.rank then
        t.nodes.(ri) <- {rni with parent=rj}
      else begin
        t.nodes.(rj) <- {rnj with parent=ri};
        if rni.rank = rnj.rank then
          t.nodes.(ri) <- {rni with rank=rni.rank+1}
      end
    end

  (* Used primarily for debugging:
   * A partition is a list of list of integers *)
  let to_partition t =
    let n = num_elements t in
    let rec aux i =
      if i >= n then []
      else if t.nodes.(i).parent = no_parent then
        (* If [i] is a root, we get the whole component *)
        let component =
          List.range 0 n
          |> List.filter ~f:(fun j -> find t j = i) in
        component :: aux (i + 1)
      else aux (i + 1) in
    aux 0

  let of_partition ?capacity p =
    let max_elt = List.join p |> List.fold ~init:(-1) ~f:max in
    let capacity = Option.value capacity ~default:(max_elt + 2) in
    let capacity = max capacity 1 in
    let t = create ~capacity in
    t.next_id <- max_elt + 1;
    List.iter p ~f:(function
      | [] -> assert false
      | e::es -> List.iter es ~f:(merge t e))

  let sexp_of_t t = [%sexp_of: int list list] (to_partition t)

  let t_of_sexp t = of_partition ([%of_sexp: int list list] t)

end


open Base

(* type symbol = string

type eclass =
  { mutable id: int
  ; mutable nodes: eterm list
  ; mutable parents: (eterm * eclass_ptr) list
  }

and eclass_ptr = eclass Union_find.t

and eterm = { head: symbol; args: eclass_ptr list } *)

let test_union_find () =
  let open UnionFind in
  let n = 10 in
  let d = 3 in
  let t = create ~capacity:0 in
  let ids = List.init n ~f:(fun _ -> fresh_id t) in
  List.iter ids ~f:(fun i ->
    List.iter ids ~f:(fun j ->
      if i % d = j % d then merge t i j
    ));
  sexp_of_t t |> Sexp.to_string_hum |> Stdio.print_endline

let () = test_union_find ()