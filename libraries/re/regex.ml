open Base

let test1 () =
  let re = Re.Pcre.re "([^:]*):\\s*(.*)" in
  let str = "key: value" in
  let ms = Re.all (Re.compile re) str in
  ms |> [%show: (Re.Group.t) list] |> Stdio.print_endline;
  List.map ~f:(fun g -> Re.Group.get g 1) ms
    |> [%show: string list] |> Stdio.print_endline

(* Re is not supposed to support Unicode, but simple cases like this one should work *)
let test_unicode () =
  let re = Re.Pcre.re "(a|b)±(c|d)" in
  let str = "a±d" in
  Re.execp (Re.compile re) str |> [%show: bool] |> Stdio.print_endline

let () = test_unicode ()