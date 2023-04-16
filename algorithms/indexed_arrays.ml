module type INDEX = sig
  type t
  val make_fresh: unit -> (unit -> t)
  module Array: sig
    type 'a t
    val make: int -> 'a -> 'a t
  end
  val ( .%( ) ): 'a Array.t -> t -> 'a
  val ( .%( )<- ): 'a Array.t -> t -> 'a -> unit
end

module [@inline] Index () : INDEX = struct
  include Int
  let make_fresh () =
    let i = ref (-1) in
    fun () -> (incr i; !i)
  module Array = Array
  external ( .%( ) ): 'a array -> int -> 'a = "%array_unsafe_get"
  external ( .%( )<- ): 'a array -> int -> 'a -> unit = "%array_unsafe_set"
end

module Lid = Index ()

module Lid_newtype = struct
  type t = Lid of int
  let make_fresh () =
    let i = ref (-1) in
    fun () -> (incr i; Lid !i)
    module Array = Array
    let ( .%( ) ) arr (Lid idx) = arr.(idx)
    let ( .%( )<- ) arr (Lid idx) v = arr.(idx) <- v
end

let make_fresh () =
  let i = ref (-1) in
  fun () -> (incr i; !i)

let test_int_ids =
  let fresh = make_fresh () in
  let id = fresh () in
  let arr = Array.make 1 0 in
  fun () -> arr.(id) <- arr.(id) + 1

let test_opaque_ids =
  let open Lid in
  let fresh = make_fresh () in
  let id = fresh () in
  let arr = Array.make 1 0 in
  fun () -> arr.%(id) <- arr.%(id) + 1

let test_newtype_ids =
  let open Lid_newtype in
  let fresh = make_fresh () in
  let id = fresh () in
  let arr = Array.make 1 0 in
  fun () -> arr.%(id) <- arr.%(id) + 1

let profile () =
  let open Core_bench in
  let benchs = [
    Bench.Test.create ~name:"Normal" test_int_ids;
    Bench.Test.create ~name:"Opaque" test_opaque_ids;
    Bench.Test.create ~name:"New type" test_newtype_ids] in
  benchs |> Bench.make_command |> Command_unix.run

let () = profile ()