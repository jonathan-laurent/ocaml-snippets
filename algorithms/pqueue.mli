type priority = int

type 'a t

val empty: 'a t

val singleton: priority -> 'a -> 'a t

val insert: 'a t -> priority -> 'a -> 'a t

val extract: 'a t -> priority * 'a * 'a t

val size: 'a t -> int

exception Queue_is_empty

module Mutable: sig
  type 'a t
  val create: unit -> 'a t
  val push: 'a t -> priority -> 'a -> unit
  val pop: 'a t -> priority * 'a
end