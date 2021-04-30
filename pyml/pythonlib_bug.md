# How to build the example

1. Clone the lib: `git clone https://github.com/janestreet/pythonlib.git`
1. Install the python development libraries: `sudo apt-get install python3-dev`
2. Set the right path to your Python lib folder in `examples/init/dune`
3. If you are using OCaml >4.08: apply the patch below on `examples/toploop_bindings.ml`
4. Run `dune build examples/ocaml.bc.so`

## Patch

```diff
diff --git a/examples/toploop_bindings.ml b/examples/toploop_bindings.ml
index cf1d853..bad41b8 100644
--- a/examples/toploop_bindings.ml
+++ b/examples/toploop_bindings.ml
@@ -102,12 +102,15 @@ let toploop_eval str ~verbose =
   | exn -> raise (Py.Err (SyntaxError, exn_to_string exn ~code:str))
 ;;
 
+let dummy_loc =
+  Warnings.({loc_start=Lexing.dummy_pos; loc_end=Lexing.dummy_pos; loc_ghost=false})
+
 let toploop_eval_and_get typerep str =
   let eval_value (type a) (typerep : a Typerep.t) =
     toploop_eval
       ~verbose:false
       (Printf.sprintf "let out : %s = (%s);;" (Py_typerep.to_ocaml typerep) str);
-    let path, _ = Env.lookup_value (Lident "out") !Toploop.toplevel_env in
+    let path, _ = Env.lookup_value ~loc:dummy_loc (Lident "out") !Toploop.toplevel_env in
     let obj = Toploop.eval_value_path !Toploop.toplevel_env path in
     Py_typerep.ocaml_to_python typerep (Caml.Obj.obj obj)
   in
@@ -117,7 +120,8 @@ let toploop_eval_and_get typerep str =
 
 let toploop_eval_and_get_no_type str =
   toploop_eval ~verbose:false (Printf.sprintf "let out = (%s);;" str);
-  let path, value_description = Env.lookup_value (Lident "out") !Toploop.toplevel_env in
+  let path, value_description =
+    Env.lookup_value ~loc:dummy_loc (Lident "out") !Toploop.toplevel_env in
   let obj = Toploop.eval_value_path !Toploop.toplevel_env path in
```