(* https://blog.veitheller.de/Pattern_Matching,_A_Thing_Of_The_Past.html *)

(* The tailrec version is at least 3x faster than the vanilla version *)
(* It is difficult to compare though as the algorithms are different *)

open Base

module Pattern (Id: Equal.S) (Ch: Equal.S) = struct

  type id = Id.t
  type ch = Ch.t
  type elt = Lit of ch | Vchar of id | Vseg of id
  type t = elt list

  module Matching = struct
    type t =
      { cvars: (id, ch) List.Assoc.t
      ; segvars: (id, ch list) List.Assoc.t }
    let empty = {cvars = []; segvars = []}
    let look_cvar m x = List.Assoc.find m.cvars ~equal:Id.equal x
    let look_cseg m x = List.Assoc.find m.segvars ~equal:Id.equal x
    let add_cvar m x v = {m with cvars=List.Assoc.add m.cvars ~equal:Id.equal x v}
    let add_segvar m x v = {m with segvars=List.Assoc.add m.segvars ~equal:Id.equal x v}
  end

  let rec match_string pat str =
    let open Sequence.Let_syntax in
    let fail = Sequence.empty in
    match pat, str with
    | [], [] -> return Matching.empty
    | [], _ -> fail
    | (Lit c)::pat, [] -> fail
    | (Lit c)::pat, c'::str ->
      if Ch.equal c c' then match_string pat str
      else fail
    | (Vchar _)::_, [] -> fail
    | (Vchar x)::pat, c'::str ->
      let%bind m = match_string pat str in
      begin match Matching.look_cvar m x with
        | Some c when Ch.equal c c' -> return m
        | Some c -> fail
        | None -> return (Matching.add_cvar m x c')
      end
    | (Vseg x)::pat, str ->
      let n = List.length str in
      Sequence.range ~start:`inclusive 0 ~stop:`inclusive n
      |> Sequence.map ~f:(fun k ->
        let seg', str = List.split_n str k in
        let%bind m = match_string pat str in
        begin match Matching.look_cseg m x with
          | Some seg when List.equal (Ch.equal) seg seg' -> return m
          | Some seg -> fail
          | None -> return (Matching.add_segvar m x seg')
        end)
      |> Sequence.join

  (* Like a Sequence *)
  type 'a cont = unit -> 'a res
  and 'a res = Elt of 'a * 'a cont | Done

  let match_string_fast pat str =
    (* cont: to do next *)
    (* m: current matching *)
    let rec match_fun pat str m cont =
      begin match pat, str with
      | [], [] -> Elt (m, cont)
      | [], _ -> cont ()
      | (Lit c)::pat, c'::str when Ch.equal c c' ->
        match_fun pat str m cont
      | (Lit c)::pat, _ -> cont ()
      | (Vchar _)::_, [] -> cont ()
      | (Vchar x)::pat, c'::str ->
        begin match Matching.look_cvar m x with
          | Some c when Ch.equal c c' -> match_fun pat str m cont
          | Some c -> cont ()
          | None -> match_fun pat str (Matching.add_cvar m x c') cont
        end
      | (Vseg x)::pat, str ->
        let n = (List.length str) in
        let rec aux i =
          if i > n then cont ()
          else
            let seg', str = List.split_n str i in
            begin match Matching.look_cseg m x with
              | Some seg when List.equal (Ch.equal) seg seg' ->
                match_fun pat str m (fun () -> aux (i + 1))
              | Some seg -> aux (i + 1)
              | None ->
                match_fun pat str (Matching.add_segvar m x seg')
                (fun () -> aux (i + 1))
            end in
        aux 0
      end in
    let gen = match_fun pat str Matching.empty (fun () -> Done) in
    Sequence.unfold ~init:gen ~f:(function
      | Done -> None
      | Elt (x, gen) -> Some (x, gen ()))

end

module StringPattern = struct

  include Pattern (Char) (Char)

  let elt_of_string str =
    begin match String.length str with
      | 1 -> Lit str.[0]
      | 2 ->
        begin match str.[0] with
          | '?' -> Vchar str.[1]
          | '!' -> Vseg str.[1]
          | _ -> failwith "Invalid qualifier"
        end
      | _ -> failwith "Invalid pattern"
    end

  let of_string str =
  String.split ~on:' ' str
  |> List.map ~f:elt_of_string

end

let match_string_gen match_string pat str =
  let module P = StringPattern in
  match_string
    (P.of_string pat)
    (String.to_list str |> List.filter ~f:(fun x -> Char.(x <> ' ')))

let match_string = match_string_gen StringPattern.match_string

let match_string_fast = match_string_gen StringPattern.match_string_fast

let print_matching (m: StringPattern.Matching.t) =
  List.iter m.cvars ~f:(fun p -> Stdio.print_endline ([%show: (char * char)] p));
  List.iter m.segvars ~f:(fun p -> Stdio.print_endline ([%show: (char * char list)] p));
  Stdio.print_endline ""

let profile pat str =
  let ms = match_string_fast pat str in
  Sequence.iter ms ~f:print_matching;
  let open Core_bench in
  let benchs =
    [ Bench.Test.create ~name:"Vanilla"
        (fun () -> match_string pat str |> Sequence.to_list)
    ; Bench.Test.create ~name:"Tailrec"
        (fun () -> match_string_fast pat str |> Sequence.to_list) ] in
  benchs |> Bench.make_command |> Command_unix.run

let () =
  (* profile "A !B ?C ?C !B !E" "A X Y Q Q X Y Z Z X Y Q Q X Y R" *)
  profile "!X !Y !Z" "A A A A"