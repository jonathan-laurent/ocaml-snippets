type bigint = Z.t 
let sexp_of_bigint i = String.sexp_of_t (Z.to_string i)
let bigint_of_sexp s = Z.of_string (String.t_of_sexp s)

type rational = Q.t = {num: bigint; den: bigint} [@@deriving sexp]

let () =
  Q.(of_int 1 / of_int 2)
    |> [%sexp_of: rational] |> Sexp.to_string_hum |> Stdio.print_endline