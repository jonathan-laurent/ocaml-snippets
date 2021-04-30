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
      | Alt of 'a * 'a
    let map f = function
      | Abandon -> Abandon
      | Cost (c, x) -> Cost (c, f x)
      | Alt (l, r) -> Alt (f l, f r)
  end
  include Free.Make(F)
  let abandon = Free Abandon
  let cost c = Free (F.Cost (c, return ()))
  let (<|>) lhs rhs = Free (Alt (lhs, rhs))
  module Let_syntax = struct
    let bind x ~f = (>>=) x f
  end
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
    | Free F.Alt (lhs, rhs) -> push c lhs; push c rhs; None in
  Sequence.unfold_step ~init:() ~f:(fun () ->
    match step (pop ()) with
      | None -> Sequence.Step.Skip ()
      | Some x -> Sequence.Step.Yield (x, ())
      | exception Pqueue.Queue_is_empty -> Sequence.Step.Done)

(* //////////////////////////////////////////////////////////////////////////////////// *)

(* Example on a simple graph *)
let prog =
  let open Search in
  (cost 3 >>= fun () -> return "B") <|>
  (cost 1 >>= fun () ->
    (cost 4 >>= fun () -> return "C") <|>
    (cost 1 >>= fun () -> return "A"))

(* Prints: ["A"; "B"; "C"] *)
let () = run prog |> Sequence.to_list |> [%show: string list] |> Stdio.print_endline