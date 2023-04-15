open Base
open Core_bench

module type INDEXED_ARRAY = sig
  type idx
  type 'a t
  val create: len:int -> 'a -> 'a t
  val get: 'a t -> idx -> 'a
end

module type INDEX = sig
  type t
  val (=): t -> t -> bool
  val (<>): t -> t -> bool
  val to_string: t -> string (* *)
  val generator: unit -> (unit -> t)
  module Array: INDEXED_ARRAY with type idx := t
  val ( .%( ) ): 'a Array.t -> t -> 'a
  val ( .%( )<- ): 'a Array.t -> t -> 'a -> unit
end

module [@inline always] Index () : INDEX = struct
  include Int
  let generator () =
    let i = ref (-1) in
    fun () -> (Int.incr i; !i)
  module Array = Array
  let ( .%( ) ) arr idx = arr.(idx)
  let ( .%( )<- ) arr idx v = arr.(idx) <- v
end

let generator () =
  let i = ref (-1) in
  fun () -> (Int.incr i; !i)

module Lid = Index ()

let with_int_ids ?(n=10) () =
  let fresh = generator () in
  let ids = List.init n ~f:(fun _ -> fresh ()) in
  let arr = Array.create ~len:n 0 in
  List.iter ids ~f:(fun i -> arr.(i) <- arr.(i) + 1)

let with_opaque_ids ?(n=10) () =
  let open Lid in
  let fresh = generator () in
  let ids = List.init n ~f:(fun _ -> fresh ()) in
  let arr = Array.create ~len:n 0 in
  List.iter ids ~f:(fun i -> arr.%(i) <- arr.%(i) + 1)

let simpler_with_int_ids =
  let fresh = generator () in
  let id = fresh () in
  let arr = Array.create ~len:2 0 in
  fun () -> arr.(id) <- arr.(id) + 1

let simpler_with_opaque_ids =
  let open Lid in
  let fresh = generator () in
  let id = fresh () in
  let arr = Array.create ~len:2 0 in
  fun () -> arr.%(id) <- arr.%(id) + 1

let profile ~normal ~opaque =
  let benchs = [
    Bench.Test.create ~name:"Normal" normal;
    Bench.Test.create ~name:"Opaque" opaque] in
  benchs |> Bench.make_command |> Command_unix.run

let () =
profile ~normal:with_int_ids ~opaque:with_opaque_ids;
profile ~normal:simpler_with_int_ids ~opaque:simpler_with_opaque_ids;