(* To see a more flexile implementation:
 * https://gist.github.com/VHarisop/78b3bb5e4766803f3f6ca5216c9104f8 *)

(* Union find: tree where each node points to its parent *)
type t = int option array

let create n = Array.create ~len:n None

let rec find_root t i =
  match t.(i) with
  | None -> i
  | Some j ->
    let r = find_root t j in
    t.(i) <- Some r;
    r

let equiv t i j = find_root t i = find_root t j

let merge t i j =
  let ri = find_root t i in
  let rj = find_root t j in
  if ri <> rj then begin
    t.(ri) <- Some rj
  end

let partition t =
  let n = Array.length t in
  let rec aux i =
    if i >= n then []
    else if Option.is_none t.(i) then
      (* If [i] is a root, we get the whole component *)
      let component =
        List.range 0 n
        |> List.filter ~f:(fun j -> find_root t j = i) in
      component :: aux (i + 1)
    else aux (i + 1) in
  aux 0

let () =
  let n = 10 in
  let d = 3 in
  let t = create n in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if i % d = j % d then merge t i j
    done
  done;
  partition t |> [%show: int list list] |> Stdio.print_endline