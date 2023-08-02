(* Refs:
   https://www.cs.cmu.edu/~fp/courses/15312-f02/handouts/19-objects.pdf
   https://journal.stuffwithstuff.com/2013/08/26/what-is-open-recursion/ *)

module Naive = struct
  type counter =
    { mutable get: unit -> int
    ; mutable set: int -> unit
    ; mutable incr: unit -> unit }

  let mkcounter () =
    let count = ref 0 in
    let get () = !count in
    let set n = count := n in
    let incr () = set (get () + 1) in
    {get; set; incr}

  (* Does not work. *)
  let inherited =
    let counter = mkcounter () in
    counter.set <- (fun v -> print_string "!" ; counter.set v)
end

module Working = struct
  type counter =
    { get: counter -> unit -> int
    ; set: counter -> int -> unit
    ; incr: counter -> unit -> unit }

  let mkcounter () =
    let count = ref 0 in
    let get _self () = !count in
    let set _self n = count := n in
    let incr self () = self.set self (self.get self () + 1) in
    {get; set; incr}

  let%expect_test "inherited" =
    let super = mkcounter () in
    let rec counter =
      { super with
        get=
          (fun self () ->
            let v = super.get self () in
            Printf.printf "GET: %d\n" v ;
            v )
      ; set=
          (fun self v ->
            let before = self.get self () in
            super.set counter v ;
            let after = self.get self () in
            Printf.printf "SET: %d -> %d\n" before after ) }
    in
    counter.incr counter () ;
    [%expect {|
    GET: 0
    GET: 0
    GET: 1
    SET: 0 -> 1 |}]
end

module Simpler = struct
  type counter = {get: unit -> int; set: int -> unit; incr: unit -> unit}

  let counter_class self =
    let count = ref 0 in
    let get () = !count in
    let set n = count := n in
    let incr () = self.set (self.get () + 1) in
    {get; set; incr}

  (* Cannot compile in OCaml! *)
  (* let new_counter () =
     let self = counter_class self in
     self *)
end

module SimplerFixed = struct
  type state = int ref

  type counter =
    { mutable get: unit -> int
    ; mutable set: int -> unit
    ; mutable incr: unit -> unit }

  let dummy_counter () =
    {get= (fun () -> 0); set= (fun _ -> ()); incr= (fun _ -> ())}

  let set_counter_class st self =
    let get () = !st in
    let set n = st := n in
    let incr () = self.set (self.get () + 1) in
    self.get <- get ;
    self.set <- set ;
    self.incr <- incr

  let new_counter () =
    let st = ref 0 in
    let self = dummy_counter () in
    set_counter_class st self ; self

  let set_inherited_class r self =
    let super = dummy_counter () in
    set_counter_class r super ;
    set_counter_class r self ;
    self.get <-
      (fun () ->
        let v = super.get () in
        Printf.printf "GET: %d\n" v ;
        v ) ;
    self.set <-
      (fun v ->
        let before = self.get () in
        super.set v ;
        let after = self.get () in
        Printf.printf "SET: %d -> %d\n" before after )

  let new_inherited () =
    let st = ref 0 in
    let self = dummy_counter () in
    set_inherited_class st self ;
    self

  let%expect_test "inherited_simple" =
    let c = new_inherited () in
    c.incr () ;
    [%expect {|
      GET: 0
      GET: 0
      GET: 1
      SET: 0 -> 1 |}]
end
