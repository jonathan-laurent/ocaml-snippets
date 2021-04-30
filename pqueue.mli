type priority = int

type 'a t

val empty: 'a t

val singleton: priority -> 'a -> 'a t

val insert: 'a t -> priority -> 'a -> 'a t

val extract: 'a t -> priority * 'a * 'a t

val size: 'a t -> int

exception Queue_is_empty