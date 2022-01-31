(* ////////////////////////////////////////////////////////////////////////// *)
(* // Search Monad                                                         // *)
(* ////////////////////////////////////////////////////////////////////////// *)

module MakeSearch (Probe: sig type t end) (Summary: sig type t end) =
struct

  type +'a tree =
    | Fail: string -> 'a tree
    | Pure: 'a -> 'a tree
    | Choice: {
        probe: Probe.t;
        choices: 'b choice list;
        cont: 'b -> 'a tree } -> 'a tree

  and 'b choice = {
    item: 'b;
    summary: Summary.t;
    cost: int }

  (* Naive quadratic implementation *)

  let naive_return x = Pure x

  let rec naive_bind mx f =
    match mx with
    | Fail msg -> Fail msg
    | Pure x -> f x
    | Choice {probe; choices; cont} ->
      Choice {probe; choices; cont = fun x -> naive_bind (cont x) f}

  (* More efficient implementation using the Codensity monad *)

  type 'a t = {run_cont: 'r. (('a -> 'r tree) -> 'r tree)}

  let return x = {run_cont = fun k -> k x}

  let bind {run_cont} f = {run_cont =
    fun k -> run_cont (fun x -> (f x).run_cont k)}

  let search_tree {run_cont} = run_cont (fun x -> Pure x)

  (* Utilities *)

  let (let*) = bind

  let choose ~probe choices = {run_cont = fun k ->
    Choice {probe; choices; cont = fun b -> k b}}

  let fail msg = {run_cont = fun _ -> Fail msg}

  let ensure ?(msg="") pred =
    if not pred then fail msg else return ()

  (* Example of a search algorithm: Dijkstra *)

  let dijkstra program =
    let open Pqueue.Mutable in
    let q = Pqueue.Mutable.create () in
    let step (c, s) =
      match s with
      | Pure x -> Some x
      | Fail _ -> None
      | Choice {probe; choices; cont} ->
        List.iter choices ~f:(fun {cost; item} ->
          push q (c + cost) (cont item)
        ); None in
    push q 0 (search_tree program);
    Sequence.unfold_step ~init:() ~f:(fun () ->
      match step (pop q) with
        | None -> Sequence.Step.Skip ()
        | Some x -> Sequence.Step.Yield (x, ())
        | exception Pqueue.Queue_is_empty -> Sequence.Step.Done)

end

(* Simple example *)

module Search = MakeSearch (struct type t = unit end) (struct type t = unit end)

open Search

let choose xs =
  let choices = List.map xs ~f:(fun x -> {item=x; summary=(); cost=0}) in
  choose ~probe:() choices

let example =
  let* x = choose [3; 2; 1] in
  let* y = choose [x; -x] in
  let* _ = ensure (x + y = 0) in
  return (x, y)

(* Prints: [(1, -1); (2, -2); (3, -3)] *)
let () = dijkstra example
  |> Sequence.to_list |> [%show: (int*int) list] |> Stdio.print_endline
