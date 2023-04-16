let main () =
  let arr = Array.make 1 0 in
  Array.unsafe_set arr 0 1;
  print_int arr.(0)

let () = main ()
