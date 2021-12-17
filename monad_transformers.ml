module type MONAD =
sig
  type +'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module MonadOps (M: MONAD) =
struct
  let return = M.return
  let (>>=) = M.bind
  let (let*) = M.bind
  let map f mx = mx >>= (fun x -> return (f x))
  let rec sequence = function
    | [] -> return []
    | mx::mxs ->
      let* x = mx in
      let* xs = sequence mxs in
      return (x::xs)
end

module Identity: (MONAD with type 'a t = 'a) =
struct
  type 'a t = 'a
  let return = Fn.id
  let bind x f = f x
end

module OptionT (M: MONAD):
sig
  include (MONAD with type 'a t = 'a option M.t)
  val lift: 'a M.t -> 'a t
end =
struct
  module M_ops = MonadOps(M)
  type 'a t = 'a option M.t
  let return x = M.return (Some x)
  let bind mox f = M.bind mox (fun ox ->
    match ox with
    | None -> M.return None
    | Some x -> f x)
  let lift mx = M_ops.map (fun x -> Some x) mx
end

module OptionExample =
struct
  module Opt = OptionT (Identity)
  open MonadOps (Opt)
  let example: int Opt.t =
    let* x = return 1 in
    let* y = None in
    return (x + y)
end

module StateT (M: MONAD) (S: sig type t end):
sig
  include MONAD
  val read: S.t t
  val write: S.t -> unit t
  val lift: 'a M.t -> 'a t
  val run_state: 'a t -> S.t -> ('a * S.t) M.t
end =
struct
  module M_ops = MonadOps(M)
  type 'a t = State of (S.t -> ('a * S.t) M.t)
  let return x = State (fun st -> M.return (x, st))
  let bind (State trans) f =
    State (fun st ->
      M.bind (trans st) (fun (x, st) ->
        let (State trans) = f x in
        trans st))
  let read = State (fun st -> M.return (st, st))
  let write st = State (fun _ -> M.return ((), st))
  let lift mx = State (fun st -> M_ops.map (fun x -> (x, st)) mx)
  let run_state (State trans) = trans
end

module StateExample =
struct
  module State = StateT (Identity) (struct type t = int end)
  open State
  open MonadOps (State)
  let fresh_identifier =
    let* id = read in
    let* _ = write (id + 1) in
    return ("x_" ^ Int.to_string id)
  let generate_expr =
    let* x = fresh_identifier in
    let* y = fresh_identifier in
    return [x, y]
  let ids, _ = run_state generate_expr 0
end

module ListT (M: MONAD):
sig
  include MONAD with type 'a t = 'a list M.t
  val choose: 'a list -> 'a t
  val (<|>): 'a t -> 'a t -> 'a t
  val lift: 'a M.t -> 'a t
end =
struct
  module M_ops = MonadOps (M)
  type 'a t  = 'a list M.t
  let return x = M.return [x]
  let bind mxs f =
    let open MonadOps (M) in
    let* xs = mxs in
    let rec aux = function
      | [] -> return []
      | x::xs ->
        let* y = f x in
        let* ys = aux xs in
        return (y @ ys)
    in aux xs
  let bind_alt mxs f =
    let open MonadOps (M) in
    let* xs = mxs in
    List.map xs ~f
    |> sequence
    |> map (List.concat)
  let choose xs = M.return xs
  let (<|>) mxs mys =
    let open M_ops in
    let* xs = mxs in
    let* ys = mys in
    return (xs @ ys)
  let lift mx = M_ops.map (fun x -> [x]) mx
end

module ListExample =
struct
  module List = ListT (Identity)
  open MonadOps (List)
  let choices: int list =
    let*x = [1; 2; 3] in
    let*y = [10; 20; 30] in
    return (x + y)
end

module NondetT (M: MONAD):
sig
  type 'a t = 'a node M.t
  and 'a node = Nil | Cons of 'a * (unit -> 'a t)
  include (MONAD with type 'a t := 'a t)
  val lift: 'a M.t -> 'a t
  val choose: 'a list -> 'a t
  val (<|>): 'a t -> 'a t -> 'a t
end =
(* A monad for performing lazy depth-first search. *)
struct
  module M_ops = MonadOps (M)
  type 'a t = 'a node M.t
  and 'a node = Nil | Cons of 'a * (unit -> 'a t)
  let nil = M.return Nil
  let return x = M.return (Cons (x, fun () -> nil))
  let rec append mxs mxs' =
    let open M_ops in
    let* xs = mxs in
    match xs with
    | Nil -> mxs'
    | Cons (x, mk_rest) ->
      return (Cons (x, fun () -> append (mk_rest ()) mxs'))
  let (<|>) = append
  let rec bind mxs f =
    let open M_ops in
    let* xs = mxs in
    match xs with
    | Nil -> return Nil
    | Cons (x, mk_rest) ->
      append (f x) (bind (mk_rest ()) f)
  let rec choose = function
    | [] -> nil
    | x::xs -> M.return (Cons (x, fun () -> choose xs))
  let lift mx = M_ops.map (fun x -> Cons (x, fun () -> nil)) mx
end

module type MONOID =
sig
  type t
  val neutral: t
  val compose: t -> t -> t
end

module WriterT (M: MONAD) (W: MONOID):
sig
  include MONAD
  val tell: W.t -> unit t
  val lift: 'a M.t -> 'a t
  val run_writer: 'a t -> ('a * W.t) M.t
end = struct
  module M_ops = MonadOps (M)
  type 'a t = ('a * W.t) M.t
  let return x = M.return (x, W.neutral)
  let bind mx f =
    let open M_ops in
    let* (x, w) = mx in
    let* (y, w') = f x in
    return (y, W.compose w w')
  let tell w = M.return ((), w)
  let run_writer = Fn.id
  let lift mx = M_ops.map (fun x -> (x, W.neutral)) mx
end

module WriterNondetExample =
struct
  module Log =
    struct
      type t = string list
      let neutral = []
      let compose = (@)
    end
  module Nondet = NondetT (Identity)
  module M = WriterT (Nondet) (Log)
  open M
  open MonadOps(M)
  let choose xs = lift (Nondet.choose xs)
  let rec to_list = function
    | Nondet.Nil -> []
    | Nondet.Cons (x, next) -> x :: to_list (next ())
  let search =
    let* x = choose ["x"; "y"] in
    let* _ = tell [x] in
    let* y = choose ["a"; "b"] in
    let* _ = tell [y] in
    return (x ^ y)
  let () =
    run_writer search
    |> to_list
    |> [%show: (string * string list) list]
    |> Stdio.print_endline
end

module Search: MONAD =
struct

  (* Tree monad: https://github.com/sebfisch/tree-monad *)
  (* http://sebfisch.github.io/tree-monad/ *)
  (* https://github.com/sebfisch/tree-monad/blob/master/Control/Monad/SearchTree.hs *)

  type 'a t =
    | Pure of 'a
    | Choice of (unit -> 'a t) list

  let return x = Pure x

  let rec bind mx f =
    match mx with
    | Pure x -> f x
    | Choice choices ->
      Choice (List.map choices ~f:(fun make_tree ->
        fun () -> bind (make_tree ()) f))
end

module FastSearch: MONAD =
struct
  type 'a tree =
    | Pure of 'a
    | Choice of (unit -> 'a tree) list
  type 'a t = {search: 'r. (('a -> 'r tree) -> 'r tree)}
  let return x = {search = fun k -> k x}
  let bind {search} f = {search = fun k -> search (fun x -> (f x).search k)}
  let search_tree {search} = search (fun x -> Pure x)
  let choices xs = {search = fun k ->
    Choice (List.map xs ~f:(fun x -> fun () -> k x))}
end

module SearchT (Query: sig type t end) (M: MONAD) =
struct

end

(*
    MaybeT    m a  =  m (Maybe a)
    StateT  s m a  =  s -> m (a, s)
    ReaderT r m a  =  r -> m a
    WriterT w m a  =  m (a, w)

    https://ro-che.info/articles/2012-01-02-composing-monads

    ReaderT r (WriterT w Identity) a  =  r -> (a, w)
    WriterT w (ReaderT r Identity) a  =  r -> (a, w)

    These two monads commute: it does not matter in what
    order they are used. Let's look at the state monad now.

    StateT s (WriterT w Identity) a  =  s -> ((a, s), w)
    WriterT w (StateT s Identity) a  =  s -> ((a, w), s)

    These are isomorphic too so this is not a problem.
    ReaderT, WriterT and StateT commute. Together, they form the RWS monad

    RWS r w s a  =  r -> s -> (a, s, w)

    Let's investigate MaybeT and WriterT now:

    MaybeT (WriterT w Identity) a  =  (Maybe a, w)
    WriterT w (MaybeT Identity) a  =  Maybe (a, w)

    In the first case, we can always access the log.
    Not in the second case, so it seems better to use
    MaybeT (WriterT w Identity) a, which is more powerful.

    Let's look at the list now.

    ListT m a  =  m [a]

    Let's compose with writer:

    ListT (WriterT w Identity) a  =  ([a], w)
    WriterT w (ListT Identity) a  =  [(a, w)]

    In the first case, we have a global writer.
    In the second case, we have local writers.

    Let's examine how state and SearchT interact now

    StateT s (SearchT Identity) a
        =  s -> fix tree. (a, s) + (unit -> tree) list
*)