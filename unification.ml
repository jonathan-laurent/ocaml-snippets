open Base_quickcheck

type id = string [@@deriving sexp, equal]

type term =
  | Var of id
  | App of id * term list
[@@deriving sexp]

type subst = (id * term) list [@@deriving sexp]

let get s x = List.Assoc.find s ~equal:equal_id x

let rec apply_subst s = function
  | Var x -> (get s x) |> Option.value ~default:(Var x)
  | App (f, args) -> App (f, args |> List.map ~f:(apply_subst s))

let rec variables = function
  | Var x -> [x]
  | App (f, args) -> args |> List.map ~f:variables |> List.concat_no_order

(* Compose a substitution with a mono-substitution *)
let subst_add s (x, t) =
  List.map s ~f:(fun (x', t') -> (x', apply_subst [(x, t)] t')) @ [(x, t)]

(* Compose two substitutions (in the specified order) *)
let subst_compose s s' = List.fold ~init:s ~f:subst_add s'

let rec unify t t' =
  match t, t' with
  | App (f, args), App (f', args') ->
    if equal_id f f' then unify_list args args'
    else None
  | Var x, other | other, Var x ->
    if List.mem (variables other) ~equal:equal_id x  then None
    else Some [(x, other)]

and unify_list args args' =
  match args, args' with
  | [], [] -> Some []
  | [], _::_ | _::_, [] -> assert false
  | a::args, a'::args' ->
    begin match unify a a' with
      | Some s ->
        let args = List.map ~f:(apply_subst s) args in
        let args' = List.map ~f:(apply_subst s) args' in
        unify_list args args' |> Option.map ~f:(fun s' -> subst_compose s s')
      | None -> None
    end

let rec sequence_generators =
  let open Generator.Let_syntax in
  function
    | [] -> return []
    | mx::mxs ->
      let%bind x = mx in
      let%bind xs = sequence_generators mxs in
      return (x :: xs)

let term_generator vsymb fsymb =
  let open Generator.Let_syntax in
  Generator.recursive_union
    [Generator.of_list (List.map ~f:(fun x -> Var x) vsymb)]
    ~f:(fun self -> [
      let%bind (f, arr) = Generator.of_list fsymb in
      let%bind args = Generator.list_with_length ~length:arr self in
      return (App (f, args))
    ])

let rec term_to_string = function
  | Var x -> x
  | App (f, args) ->
    let args_str = args
      |> List.map ~f:term_to_string
      |> String.concat ~sep:", " in
    f ^ "(" ^ args_str ^ ")"

let subst_to_string s =
  let sub (from, repl) = from ^ " -> " ^ term_to_string repl in
  List.map ~f:sub s
  |> String.concat ~sep:", "
  |> Printf.sprintf "{%s}"

(* Running some tests *)

let generate_term () =
  let vsymb = ["x"; "y"; "z"] in
  let fsymb = [("c", 0); ("f", 2); ("g", 2); ("h", 1)] in
  let gen = term_generator vsymb fsymb in
  let random = Splittable_random.State.create (Random.State.default) in
  Generator.generate ~size:1 ~random gen
  |> term_to_string |> Stdio.print_endline

let generate_terms () =
  let vsymb = ["x"; "y"; "z"] in
  let fsymb = [("c", 0); ("f", 2); ("g", 2); ("h", 1)] in
  let gen = term_generator vsymb fsymb in
  Test.with_sample_exn gen ~f:(fun seq ->
    seq |> Sequence.iter ~f:(fun t ->
      term_to_string t |> Stdio.print_endline))

let test_subst () =
  let t = App ("f", [Var "x"; App ("+", [Var "x"; App ("1", [])])]) in
  let s = [("x", App ("g", [Var "y"]))] in
  let t' = apply_subst s t in
  t  |> term_to_string  |> Stdio.print_endline;
  s  |> subst_to_string |> Stdio.print_endline;
  t' |> term_to_string  |> Stdio.print_endline

let test_unify () =
  let x = Var "x" in
  let app f args = App (f, args) in
  let t = app "f" [x; app "g" [x; x]] in
  let t' = app "f" [app "h" [Var "y"]; app "g" [Var "z"; app "h" [app "1" []]]] in
  let s = unify t t' in
  t  |> term_to_string |> Stdio.print_endline;
  t' |> term_to_string |> Stdio.print_endline;
  s  |> Option.value_map ~default:"impossible" ~f:subst_to_string |> Stdio.print_endline

let () = test_unify ()