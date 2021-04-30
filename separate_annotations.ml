open Base

let () = Stdio.print_endline "Hello world!"

type identifier = string

type 'e construct =
  | Var of identifier
  | Metavar of identifier
  | App of identifier * 'e list

type expr = E of expr construct

let unfix (E e) = e

type 'a expr_annot = AE of 'a * ('a expr_annot) construct

let annot (AE (x, _)) = x

let ctor (AE (_, x)) = x

let rec with_positions' revpos e =
  let pos = List.rev revpos in
  match unfix e with
  | Var s | Metavar s as ctor -> AE (pos, ctor)
  | App (f, args) ->
    let args = List.mapi args (fun i a ->
      with_positions' (i::revpos) a) in
    AE (pos, App (f, args))

let with_positions = with_positions' []

let map_ctor e ~f =
  match e with
  | Var _ | Metavar _ -> e
  | App (s, args) -> App (s, List.map args f)

let rec zip_annots_with (AE (a, e)) (AE (a', e')) ~f =
  let ctor =
    match e, e' with
    | Var x, Var _ -> Var x
    | Metavar x, Metavar _ -> Metavar x
    | App (s, args), App (s', args') ->
      App (s, List.map2_exn args args' (zip_annots_with ~f))
    | _ -> assert false in
  AE (f a a', ctor)