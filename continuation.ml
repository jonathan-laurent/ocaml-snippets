type ('r, 'a) cont = Cont of (('a -> 'r) -> 'r)

let runCont (Cont f) = f

let return x = Cont (fun k -> k x)

let (>>=) kx f = Cont (fun k -> runCont kx (fun x -> runCont (f x) k))

let cont1 =
  return 1 >>= fun x ->
  Cont (fun k -> "aborted") >>= fun y ->
  return (x + y)

let test1 () =
  runCont cont1 Int.to_string
  |> Stdio.print_string

let cont2 =
  return 1 >>= fun x ->
  Cont (fun k -> k 2 @ k 3) >>= fun y ->
  return (x + y)

let test2 () =
  runCont cont2 (fun x -> [x])
  |> [%show: int list]
  |> Stdio.print_string

let () = test2 ()

(* return x >>= f =  f x *)