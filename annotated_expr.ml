type identifier = string [@@deriving eq]

type 'a expr =
  { body: 'a expr_body
  ; annot: 'a [@equal fun _ _ -> true] }
[@@deriving eq]

and 'a expr_body =
  | Var of identifier
  | Metavar of identifier
  | App of identifier * ('a expr) list
[@@deriving eq]

let map_children ~f e =
  match e.body with
  | Var _ | Metavar _ -> []
  | App (s, args) -> List.map ~f args

let set_annot e annot = {e with annot}

let set_body e body = {e with body}

let with_annot body annot = {body; annot}

let rec annotate_positions' revpos e =
  match e.body with
  | Var _ | Metavar _ as body ->
      with_annot body (List.rev revpos)
  | App (s, args) ->
      let args = List.mapi args ~f:(fun i e -> annotate_positions' (i::revpos) e) in
      with_annot (App (s, args)) (List.rev revpos)

let annotate_positions e = annotate_positions' [] e

let rec subst e x v =
  match e.body with
  | Var x -> e
  | Metavar x' -> if equal_string x x' then v else e
  | App (f, args) -> set_body e (App (f, List.map args ~f:(fun c -> subst c x v)))

let rec zip_annotations_with ~f e e' =
  let body =
    match e.body, e'.body with
    | Var x, Var x' ->
        assert (equal_string x x'); Var x
    | Metavar x, Metavar x' ->
        assert (equal_string x x'); Metavar x
    | App (s, args), App (s', args') ->
        assert (equal_identifier s s');
        assert (List.length args = List.length args');
        App (s, List.map2_exn args args' ~f:(zip_annotations_with ~f))
    | _ -> assert false in
  {body; annot = f e.annot e'.annot}

let zip_annotations e e' = zip_annotations_with ~f:(fun x y -> (x, y)) e e'

let test () =
  let x = Var "x" in
  let e1 = {body=x; annot="hello"} in
  let e2 = {body=x; annot=0} in
  let ae1 = annotate_positions e1 in
  let ae2 = annotate_positions e2 in
  let _ae12 = zip_annotations ae1 ae2 in
  assert ([%equal: int expr] e2 {e2 with annot=1})

let () = test ()