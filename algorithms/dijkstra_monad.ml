(* //////////////////////////////////////////////////////////////////////////////////// *)
(* // A straightforward and slow implementation of the Djikstra monad                // *)
(* //////////////////////////////////////////////////////////////////////////////////// *)

open Base

module Search = struct

  module F = struct
    type cost = int
    type 'a t =
      | Abandon
      | Cost of cost * (unit -> 'a)
      | Choice of (unit -> 'a) list
    let map f = function
      | Abandon -> Abandon
      | Cost (c, getx) -> Cost (c, fun () -> f (getx ()))
      | Choice xs -> Choice (List.map ~f:(fun getx () -> f (getx ())) xs)
  end
  include Free.Make(F)
  let abandon = Free Abandon
  let cost c = Free (Cost (c, fun () -> return ()))
  let (<|>) lhs rhs = Free (Choice [(fun () -> lhs); (fun () -> rhs)])
  let choose vs = Free (Choice (List.map ~f:(fun x -> fun () -> return x) vs))
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
    | Free F.Cost (c', getx) -> push (c + c') (getx ()); None
    | Free F.Choice getxs ->
      List.iter getxs ~f:(fun getx -> push c (getx ())); None in
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

(* Infinite recursive search.
 * This won't terminate if you replace the cost by -1 *)
let rec nats i =
  let open Search in
  return i <|> (cost 1 >>= fun () -> nats (i + 1))

(* //////////////////////////////////////////////////////////////////////////////////// *)

(* Prints: ["A"; "B"; "C"] *)
let () = run simple_graph
  |> Sequence.to_list |> [%show: string list] |> Stdio.print_endline

(* Prints: [(1, -1); (2, -2); (3, -3)] *)
let () = run let_notation
  |> Sequence.to_list |> [%show: (int*int) list] |> Stdio.print_endline

(* Prints: [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] *)
let () = run (nats 0)
  |> fun s -> Sequence.take s 10
  |> Sequence.to_list |> [%show: int list] |> Stdio.print_endline
