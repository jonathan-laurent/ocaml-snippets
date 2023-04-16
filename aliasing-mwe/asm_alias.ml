let unsafe_set arr idx v = Array.unsafe_set arr idx v [@@inline]

let main () =
  let arr = Array.make 1 0 in
  unsafe_set arr 0 1;
  print_int arr.(0)

let () = main ()
