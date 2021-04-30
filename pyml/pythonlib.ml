open Python_lib
open Python_lib.Let_syntax

let () = Py.initialize ()

let add: Py.Object.t Python_lib__Defunc.t =
  let%map_open arg1 = positional "lhs" int ~docstring:""
  and arg2 = positional "rhs" int ~docstring:"" in
  python_of_int (arg1 + arg2)

let add' = Py.Callable.of_function ~name:"add" (fun args ->
  let args = Array.map ~f:Py.Int.to_int args in
  Py.Int.of_int (args.(0) + args.(1)))

let print obj =
  let builtins = Py.Eval.get_builtins () in
  let f = Py.Dict.find_string builtins "print" |> Py.Callable.to_function in
  f [|obj|]

let ignore_py_objet (n: Py.Object.t) = ()

let _ = Pyops.(.&())

let () =
  let util = Py_module.create "util" in
  Py_module.set util "add" add;
  let kwargs = Map.empty (module String) in
  let args = Array.map ~f:Py.Int.of_int [|2; 3|] in
  Python_lib.Defunc.apply add args kwargs
  |> print
  |> ignore_py_objet;
  Py.Callable.to_function add' args
  |> print
  |> ignore_py_objet;
  let util' = Py.Import.add_module "util" in
  Py.Module.set util' "add" add';
  let open Pyops in
  util'.&("add") args
  |> print
  |> ignore_py_objet
  


(* I want a very tight integration with python *)