(* Generating uniform binary trees (see Lample's paper) *)

open Base

type bin_tree = 
  | Leaf
  | Node of bin_tree * bin_tree


(* Memoization *)

let rec memoize key_module f =
  let tab = Hashtbl.create key_module in
  fun x ->
    match Hashtbl.find tab x with
    | Some y -> y
    | None ->
      begin
        let y = f x in
        Hashtbl.set tab ~key:x ~data:y;
        y
      end

(* https://stackoverflow.com/questions/19859953/limitations-of-let-rec-in-ocaml *)
let fix_memoize key_module f =
  let rec fix = lazy (memoize key_module (fun x -> f (Lazy.force fix) x)) in
  Lazy.force fix


(* Sampling from a distribution *)

module Distribution : sig 

  type 'a t

  val make: ('a * float) list -> 'a t

  val sample: 'a t -> 'a

end = struct

  type 'a t = ('a * float) list

  let sum_coeffs d =
    List.fold d ~init:0.0 ~f:(fun acc (_, p) -> acc +. p)

  let normalize d =
    let open Float in
    let s = sum_coeffs d in
    assert (s > 0.);
    List.map d ~f:(fun (x, p) -> (x, p / s))

  let make d =
    let open Float in
    assert (List.for_all d ~f:(fun (_, p) -> p >= 0.));
    normalize d
  
  let sample d =
    let open Float in
    let p = Random.float 1.0 in
    let rec search pcum = function
      | [] -> assert false
      | (e, _)::[] -> e
      | (e, pe)::d ->
        let pcum = pcum + pe in
        if pcum >= p then e else search pcum d in
    search 0.0 d

end

let number_trees' self n =
  if n = 0 then 1.0 else
  List.range 0 n
  |> List.map ~f:(fun k -> self k *. self (n - 1 - k))
  |> List.fold ~init:0.0 ~f:(+.)

let number_trees = fix_memoize (module Int) number_trees'

let rec sample_tree n =
  if n = 0 then Leaf else
  let prob k = number_trees k *. number_trees (n - k - 1) in
  let k = 
    List.range 0 n
    |> List.map ~f:(fun k -> (k, prob k))
    |> Distribution.make
    |> Distribution.sample in
  Node (sample_tree k, sample_tree (n - k - 1))


(* Ascii art utility *)

module Box : sig 

  type 'a t

  val size: 'a t -> int * int

  val width: 'a t -> int

  val height: 'a t -> int

  val get: 'a t -> int * int -> 'a option

  val make: int * int -> f:(int -> int -> 'a) -> 'a t

  val pad_right: empty:'a -> 'a t -> 'a t

  val pad_bot: empty:'a -> 'a t -> 'a t

  val append_vertical: empty:'a -> 'a t -> 'a t -> 'a t

  val append_horizontal: empty:'a -> 'a t -> 'a t -> 'a t

  val map: f:('a -> 'b) -> 'a t -> 'b t

  val to_string: string t -> string

end = struct

  type 'a t = 'a list list (* list of lines *)
  [@@deriving sexp]

  let size' box =
    match box with
    | [] -> Some (0, 0)
    | first::_ ->
      let w = List.length first in
      if List.for_all box ~f:(fun l -> List.length l = w) then
        Some (w, List.length box)
      else None

  let check_box box = size' box |> Option.is_some

  let checking_box box = assert (check_box box); box

  let size box = size' box |> Option.value_exn

  let height box = let (w, h) = size box in h

  let width box = let (w, h) = size box in w

  let get box (col, row) =
    List.nth box row |> Option.bind ~f:(fun r -> List.nth r col)

  let make (w, h) ~f =
    List.range 0 h |> List.map ~f:(fun row ->
      List.range 0 w |> List.map ~f:(fun col -> f col row))

  let empty_box ~empty (w, h) = make (w, h) ~f:(fun _ _ -> empty)

  (* assuming same height *)
  let append_horizontal' box box' =
    assert (height box = height box');
    List.map2_exn box box' ~f:( @ )
    |> checking_box

  (* assuming same width *)
  let append_vertical' box box' =
    assert (width box = 0 || width box' = 0 || width box = width box');
    box @ box'
    |> checking_box

  let append_horizontal ~empty box box' =
    let (w, h) = size box in
    let (w', h') = size box' in
    if h <= h' then
      let padding = empty_box ~empty (w, h' - h) in
      append_horizontal' (append_vertical' box padding) box'
    else
      let padding' = empty_box ~empty (w', h - h') in
      append_horizontal' box (append_vertical' box' padding')

  let append_vertical ~empty box box' =
    let (w, h) = size box in
    let (w', h') = size box' in
    if w <= w' then
      let padding = empty_box ~empty (w' - w, h) in
      append_vertical' (append_horizontal' box padding) box'
    else
      let padding' = empty_box ~empty (w - w', h') in
      append_vertical' box (append_horizontal' box' padding')

  let pad_bot ~empty box =
    append_vertical' box (empty_box ~empty (width box, 1))

  let pad_right ~empty box =
    append_horizontal' box (empty_box ~empty (1, height box))

  let map ~f box =
    List.map box ~f:(fun l -> List.map ~f l)

  let to_string box = box
    |> List.map ~f:(String.concat)
    |> String.concat ~sep:"\n"

end


(* Using boxes to render trees *)

type tree_char = CEmpty | CNode | CLeaf | CHor | CVer
[@@deriving sexp, eq]

let rec print_tree ?(pad=true) = function
  | Leaf -> Box.make (1, 1) ~f:(fun _ _ -> CLeaf)
  | Node (lhs, rhs) ->
    let empty = CEmpty in
    let lbox = print_tree ~pad lhs in 
    let rbox = print_tree ~pad rhs in
    let lbox, rbox = if pad 
      then Box.pad_right ~empty lbox, Box.pad_bot ~empty rbox
      else lbox, rbox in
    let corner = Box.make (Box.width lbox, Box.height rbox) ~f:(fun col row ->
      if col = 0 && row = 0 then CNode
      else if col = 0 then CVer
      else if row = 0 then CHor
      else CEmpty) in
    let top = Box.append_horizontal ~empty corner rbox in
    Box.append_vertical ~empty top lbox

let string_of_tree_char = function
  | CEmpty -> " "
  | CNode  -> "@"
  | CLeaf  -> "o"
  | CHor   -> "-"
  | CVer   -> "|"

let string_of_tree_basic tree = tree
  |> print_tree ~pad:false
  |> Box.map ~f:(string_of_tree_char)
  |> Box.to_string

type 'a neighborhood = {top: 'a; bot: 'a; left: 'a; right: 'a}

let map_box_neighborhood ~outside ~neighborhood_computation ~f box =
  Box.make (Box.size box) ~f:(fun col row ->
    let g = neighborhood_computation in
    let get (c, r) = Box.get box (c, r) |> Option.value ~default:outside in
    let center = get (col, row) in
    let top = get (col, row-1) |> g in 
    let bot = get (col, row+1) |> g in
    let left = get (col-1, row) |> g in 
    let right = get (col+1, row) |> g in 
    f center {top; bot; left; right})

let string_of_tree tree =
  let not_empty c = not (equal_tree_char c CEmpty) in
  tree
  |> print_tree ~pad:true
  |> map_box_neighborhood 
    ~outside:CEmpty
    ~neighborhood_computation:not_empty
    ~f:(fun v neigh ->
      match v with
      | CEmpty -> "  "
      | CVer -> "│ "
      | CHor -> "──"
      | CLeaf -> "  "
      | CNode ->
        begin match neigh with
          | {top=true;  bot=true; right=true; left=false} -> "├─"
          | {top=false; bot=true; right=true; left=false} -> "┌─"
          | {top=false; bot=true; right=true; left=true}  -> "┬─"
          | _ -> "@@" (* Should not happen *)
        end)
  |> Box.to_string


(* Main *)

let test_rendering () = 
  let tree1 = Node (Leaf, Leaf) in
  let tree2 = Node (tree1, tree1) in
  let tree3 = Node (tree1, tree2) in
  Stdio.print_endline (string_of_tree tree3)

let test_number_trees () =
  let f n = Stdio.printf "B(%d) = %f\n" n (number_trees n) in
  f 1; f 2; f 3; f 4; f 10; f 30

let test_sample_tree ~num_internal_nodes ~num_trees =
  List.range 0 num_trees |> List.iter ~f:(fun _ ->
    let tree = sample_tree num_internal_nodes in
     (* tree |> string_of_tree_basic |> Stdio.print_endline; *)
     tree |> string_of_tree |> Stdio.print_endline)

let () = test_sample_tree ~num_internal_nodes:8 ~num_trees:5