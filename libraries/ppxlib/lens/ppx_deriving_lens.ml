open Base
open Ppxlib

let generate_impl ~ctxt
  ((_rec_flag, type_declarations): rec_flag * type_declaration list) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let (module Ast) = Ast_builder.make loc in
  match type_declarations with
  | [{ptype_name=_; ptype_kind=(Ptype_record decls); _}] ->
      List.map decls ~f:(fun {pld_name; _} ->
        let binding = Ast.pvar pld_name.txt in
        let field_ident = Ast.Located.lident pld_name.txt in
        let field = Ast.pexp_field [%expr x] field_ident in
        let updated =
          Ast.pexp_record [(field_ident, [%expr v])] (Some [%expr x]) in
        [%stri let [%p binding] = {
          Lens.get = (fun x -> [%e field]);
          Lens.set = (fun v x -> [%e updated])
        }])
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