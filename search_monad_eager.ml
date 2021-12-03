(* //////////////////////////////////////////////////////////////////////////////////// *)
(* // A straightforward and slow implementation of the Djikstra monad                // *)
(* //////////////////////////////////////////////////////////////////////////////////// *)

open Base

module Search = struct

  module F = struct
    type cost = int
    type 'a t =
      | Abandon
      | Cost of cost * 'a
      | Choice of 'a list
    let map f = function
      | Abandon -> Abandon
      | Cost (c, x) -> Cost (c, f x)
      | Choice xs -> Choice (List.map ~f xs)
  end
  include Free.Make(F)
  let abandon = Free Abandon
  let cost c = Free (F.Cost (c, return ()))
  let (<|>) lhs rhs = Free (Choice [lhs; rhs])
  let choose vs = Free (Choice (List.map ~f:return vs))
  let ensure b = if b then return () else Free Abandon
end

(* //////////////////////////////////////////////////////////////////////////////////// *)

let run search =
  let q = ref (Pqueue.singleton 0 search) in
  let push cost cont = q := Pqueue.insert !q cost cont in
  let pop () = let c, n, q' = Pqueue.extract !q in q := q'; (c, n) in
  let step (c, s) =
    let open Search in
    match s with
    | Pure x -> Some x
    | Free F.Abandon -> None
    | Free F.Cost (c', x) -> push (c + c') x; None
    | Free F.Choice xs -> List.iter xs ~f:(push c); None in
  Sequence.unfold_step ~init:() ~f:(fun () ->
    match step (pop ()) with
      | None -> Sequence.Step.Skip ()
      | Some x -> Sequence.Step.Yield (x, ())
      | exception Pqueue.Queue_is_empty -> Sequence.Step.Done)

(* //////////////////////////////////////////////////////////////////////////////////// *)

(* Example on a simple graph *)
let simple_graph =
  let open Search in
  (cost 3 >>= fun () -> return "B") <|>
  (cost 1 >>= fun () ->
    (cost 4 >>= fun () -> return "C") <|>
    (cost 1 >>= fun () -> return "A"))

(* Example using do notation *)
let let_notation =
  let open Search in
  let* x = choose [3; 2; 1] in
  let* _ = cost x in
  let* y = choose [x; -x] in
  let* _ = ensure (x + y = 0) in
  return (x, y)

(* Infinite recursive search *)
let rec nats i =
  let open Search in
  return i <|> (cost 1 >>= fun () -> nats (i + 1))

(* Prints: ["A"; "B"; "C"] *)
let () = run simple_graph |> Sequence.to_list |> [%show: string list] |> Stdio.print_endline

(* Prints: [(1, -1); (2, -2); (3, -3)] *)
let () = run let_notation |> Sequence.to_list |> [%show: (int*int) list] |> Stdio.print_endline

(* let () = Sequence.take (run (nats 0)) 10 |> Sequence.to_list |> [%show: int list] |> Stdio.print_endline *)