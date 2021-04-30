(* Bigarray tests *)

module Arr1 = Bigarray.Array1
module Arr2 = Bigarray.Array2
module Ndarray = Owl.Dense.Ndarray.Generic

let bigarray () =
  let (xdim, ydim) = (3, 4) in
  let arr = Arr2.create Bigarray.float32 Bigarray.c_layout xdim ydim in
  let _arr' = Ndarray.zeros Bigarray.float32 [|3; 4|] in
  let first_line = Arr2.slice_left arr 1 in
  for i = 0 to Arr1.dim first_line - 1 do
    first_line.{i} <- Float.of_int i
  done;
  arr |> Bigarray.genarray_of_array2 |> Ndarray.print


let slicing () =
  let open Ndarray in
  let open Bigarray in
  let arr = zeros float32 [|3; 4|] in
  arr.%{1; 1} <- 1.0;
  print arr.!{L [1; 0; 2]; R []}

let () = slicing ()