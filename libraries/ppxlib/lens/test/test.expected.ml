type my_record = {
  x: int ;
  y: float }[@@deriving lens]
include struct let f x = x end[@@ocaml.doc "@inline"][@@merlin.hide ]
