type ('r, 'a) cont = Cont of (('a -> 'r) -> 'r)

let run_cont (Cont f) = f

let return x = Cont (fun k -> k x)

let (>>=) kx f = Cont (fun k -> run_cont kx (fun x -> run_cont (f x) k))

let (let*) = (>>=)

let cont1 =
  let* x = return 1 in
  let* y = Cont (fun k -> "aborted") in
  return (x + y)

let test1 () =
  run_cont cont1 Int.to_string
  |> Stdio.print_string

let cont2 =
  let* x = return 1 in
  let* y = Cont (fun k -> k 2 @ k 3) in
  return (x + y)

let test2 () =
  run_cont cont2 (fun x -> [x])
  |> [%show: int list]
  |> Stdio.print_endline

(* let () = test2 () *)

(* ((a -> cont b) -> cont a) -> cont a *)
let callcc f = Cont (fun k -> run_cont (f (fun a -> Cont (fun _ -> k a))) k)

let use_call_cc =
  let* x = callcc (fun k ->
      let* a = k 1 in
      let* b = k 2 in
      return 0 (* we don't care what this is *)
    ) in
  return (x + 1)

let test_3 () = run_cont use_call_cc (fun x -> [%show: int] x |> Stdio.print_endline)

(* return x >>= f =  f x *)

(* Using continuations to implement generators *)

(* gen_squares *)

let make_generator gen () =
  let break = ref None in
  let resume = ref None in
  let yield v =
    callcc (fun k ->
      resume := Some k;
      Option.value_exn !break v
    ) in
  fun () ->
    run_cont (callcc (fun k ->
      break := Some k;
      match !resume with
      | Some resume -> resume ()
      | None -> gen yield)) Fn.id

let gen_squares =
  make_generator (fun yield ->
    let rec loop i =
      let* _ = yield (i * i) in
      loop (i + 1) in
    loop 1)

let gen_squares_bis =
  Sequence.unfold_step
    ~init:1
    ~f:(fun i -> Sequence.Step.Yield (i * i, i + 1))

let test_gen () =
  let g = gen_squares () in
  for i = 1 to 10 do
    g () |> [%show: int] |> Stdio.print_endline
  done

(* let () = test_gen () *)

(* Shift/reset *)
(* https://gist.github.com/sebfisch/2235780 *)

let get_cont c = run_cont c Fn.id

let capture c = return (get_cont c)

let escape f = Cont (fun k -> get_cont (f k))

let ex = capture (
  let* x = escape (fun continue ->
    return (17 + continue 4)) in
  return (2 * x))

let throw e = escape (fun _ -> return (fun h -> h e))
let try_catch f h =
  get_cont (capture (let* res = f () in return (fun _ -> res))) h

let test_exn =
  try_catch
    (fun () -> let*x = throw 42 in return (1 + x))
    (fun e -> e)

let test_delimited () =
  get_cont ex |> [%show: int] |> Stdio.print_endline

let () = test_delimited ()