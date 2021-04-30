(* Example of calling torch using Pyml *)

let array_of_bigarray t =
  let n = Bigarray.Array1.dim t in
  Array.init n ~f:(fun i -> Bigarray.Array1.get t i)

let () =
  Py.add_python_path "snippets/pyml";
  Py.initialize ();
  let network_module = Py.import "network" in
  let indim, outdim = 10, 2 in
  let mlp =
    let width = 2 in
    let depth = 2 in
    let mlp_args = [|indim; outdim; width; depth|] |> Array.map ~f:Py.Int.of_int in
    Py.Module.get_function network_module "make_mlp" mlp_args in
  let arg =
    Array.create ~len:indim 0.0
    |> Bigarray.Array1.of_array Float32 C_layout
    |> Bigarray.genarray_of_array1
    |> Numpy.of_bigarray
    |> fun t -> Py.Module.get_function network_module "to_torch_tensor" [|t|] in
  let res =
    Py.Object.call_method mlp "forward" [|arg|]
    |> fun t -> Py.Module.get_function network_module "of_torch_tensor" [|t|]
    |> Numpy.to_bigarray Float32 C_layout
    |> Bigarray.array1_of_genarray
    |> array_of_bigarray in
  [%show: float array] res |> Stdio.print_endline