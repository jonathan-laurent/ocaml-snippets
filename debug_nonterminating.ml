open Base

(* The runtime won't be able to catch the SIGINT signal
 * if the nonterminating piece of code does not allocate.
 * See https://github.com/ocaml/ocaml/issues/3747.
 * Also, the stacktrace for this example may not be satsfying when
 * the program is compiled the the -O3 flag. *)
 let rec f x _y = f (x + 1) (Some x)

 let () =
 Caml.Printexc.record_backtrace true;
 Caml.Sys.catch_break true;
   try f 0 None with
   | Caml.Sys.Break -> Caml.Printexc.print_backtrace Stdio.Out_channel.stderr