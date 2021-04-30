(* Simple example of batching requests using Lwt *)

open Base

module type BATCHIFIER = sig
  type ('a, 'b) t
  val create: ('a list -> 'b list) -> ('a, 'b) t
  val set_num_workers: ('a, 'b) t -> int -> unit
  val unsubscribe: ('a, 'b) t -> unit
  val request: ('a, 'b) t -> 'a -> 'b Lwt.t
end

module Batchifier: BATCHIFIER = struct

  type ('a, 'b) t = {
    mutable num_subscribers: int;
    waiting: ('a * 'b Lwt.u) Queue.t;
    processing: 'a list -> 'b list }

  let create processing =
    {num_subscribers = 0; waiting = Queue.create (); processing}

  let set_num_workers t v =
    t.num_subscribers <- v

  let check_queue t =
    assert (Queue.length t.waiting <= t.num_subscribers);
    if Queue.length t.waiting = t.num_subscribers then
    begin
      let xs, hs = Queue.to_list t.waiting |> List.unzip in
      Queue.clear t.waiting;
      let ys = t.processing xs in
      List.iter2_exn ys hs ~f:(fun y h -> Lwt.wakeup h y);
    end

  let unsubscribe t =
    t.num_subscribers <- t.num_subscribers - 1;
    check_queue t

  let request t x =
    let res, h = Lwt.wait () in
    Queue.enqueue t.waiting (x, h);
    check_queue t;
    res

end

let msgln = Stdio.print_endline
let sprintf = Printf.sprintf

let process xs =
  msgln (sprintf "Evaluating a batch of size %d" (List.length xs));
  List.map xs ~f:(fun x -> x * x)

let main () =
  let manager = Batchifier.create process in
  let spawn_worker id n =
    msgln (sprintf "Spawning worker %d (%d tasks)" id n);
    let rec aux i =
      if i > n then Lwt.return ()
      else
        let%lwt isq = Batchifier.request manager i in
        msgln (sprintf "Worker %d: %d^2 = %d" id i isq);
        aux (i + 1) in
    let%lwt () = aux 1 in
    let () = Batchifier.unsubscribe manager in
    Lwt.return () in
  Batchifier.set_num_workers manager 3;
  Lwt.join [spawn_worker 1 3; spawn_worker 2 2; spawn_worker 3 4]

let () = Lwt_main.run (main ())