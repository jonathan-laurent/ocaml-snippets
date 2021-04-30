open Base

module type MODULE = sig
  type t (* this type is opaque for the debugger *)
  val a: t
  val b: t
end

module Module: MODULE = struct
  type t = A | B
  let a = A
  let b = B
end

type custom = Foo | Bar of string

let () =
  let i = 42 in
  let b = true in
  let s = "hello" in
  let sl = ["hello"; "world"] in
  let bl = [true; false] in
  let il = [1; 2; 3] in
  let sl = [String.make 3 'a'; "hello"; "world"] in
  let sl' = ["aaa"; "bbb"] in
  let ba = [|true; false|] in
  let ia = [|1; 2; 3|] in
  let sa = [|"hello"; "world"|] in
  let none = None in
  let io = Some 1 in
  let so = Some "hello" in
  let sol = [None; Some "abc"] in
  let foo = Foo in
  let bar = Bar "bar" in
  let h = Module.a in
  Stdio.print_endline "Hello world"