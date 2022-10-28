open Ppxlib
open Ast_builder.Default

let generate_impl ~ctxt
  ((_rec_flag, type_declarations): rec_flag * type_declaration list) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match type_declarations with
  | [{ptype_name=_; ptype_kind=(Ptype_record _); _}] ->
      let f = [%stri let f x = x] in
      [f]
  | [_] ->
    Location.raise_errorf ~loc
      "not a valid record."
  | _ ->
    Location.raise_errorf ~loc
      "mutually recursive definitions are not allowed."

(* let generate_intf ~ctxt:_ (_rec_flag, type_declarations) =
  assert false *)

let my_deriver =
  let impl_generator = Deriving.Generator.V2.make_noarg generate_impl in
  (* let intf_generator = Deriving.Generator.V2.make_noarg generate_intf in *)
  Deriving.add
    ~str_type_decl:impl_generator
    (* ~sig_type_decl:intf_generator *)
    "lens"