module type FUNCTOR = sig
  type 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
end

module Make (F: FUNCTOR) = struct

  type 'a t =
    | Pure of 'a
    | Free of ('a t) F.t

  let return x = Pure x

  let rec map f = function
    | Pure x -> Pure (f x)
    | Free fmx -> Free (F.map (map f) fmx)

  let rec join = function
    | Pure mx -> mx
    | Free fmmx -> Free (F.map join fmmx)

  let lift fx = Free (F.map return fx)

  let (>>=) mx f = join (map f mx)

  (* Defining bind directly without join and map *)
  let rec (>>=) mx f =
    match mx with
    | Pure x -> f x
    | Free fmx -> Free (F.map (fun mx -> mx >>= f) fmx)

end