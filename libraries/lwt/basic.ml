let rec map_async ~f = function
  | [] -> Lwt.return []
  | x::xs ->
    let y = f x in
    let ys = map_async ~f xs in
    let%lwt y = y in
    let%lwt ys = ys in
    Lwt.return (y :: ys)

let rec map_serial ~f = function
  | [] -> Lwt.return []
  | x::xs ->
    let%lwt y = f x in
    let%lwt ys = map_serial ~f xs in
    Lwt.return (y :: ys)

let test_map map =
  map ~f:(fun i ->
      let%lwt () = Lwt_unix.sleep (Float.of_int i) in
      Lwt.return (Stdio.print_endline ([%show: int] i)))
  [2; 1; 3]

let main () =
  let%lwt _ : unit list = test_map map_async in
  let%lwt _ : unit list = test_map map_serial in
  Lwt.return ()

let () =
  Lwt_main.run (main ())