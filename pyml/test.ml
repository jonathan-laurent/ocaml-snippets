(* Trying some features of ppx_python *)

let python_of_int = Py.Int.of_int
let int_of_python = Py.Int.to_int
let python_of_string = Py.String.of_string
let string_of_python = Py.String.to_string

type t =
  { field_a : int
  ; field_b : string
  }
[@@deriving python]

let python_code = "
from ocaml import hello
hello('World')
"

let () =
  let () = Py.initialize () in
  let m = Py.Import.add_module "ocaml" in
  let hello args =
    Stdio.printf "Hello, %s!\n" (Py.String.to_string args.(0));
    Py.none in
  Py.Module.set m "hello" (Py.Callable.of_function hello);
  let _ : Py.Object.t = Py.Run.eval ~start:Py.File python_code in
  ()