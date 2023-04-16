let unsafe_set arr idx v = Array.unsafe_set arr idx v [@@inline]

let normal =
  let arr = Array.make 1 0 in
  fun () -> Array.unsafe_set arr 0 1

let variant =
  let arr = Array.make 1 0 in
  fun () -> unsafe_set arr 0 1

let () =
  let open Core_bench in
  let benchs = [
    Bench.Test.create ~name:"Normal" normal;
    Bench.Test.create ~name:"Variant" variant] in
  benchs |> Bench.make_command |> Command_unix.run