type my_record = {
  x: int ;
  y: float }[@@deriving lens]
include
  struct
    let x =
      {
        Lens.get = (fun x -> x.x);
        Lens.set = (fun v -> fun x -> { x with x = v })
      }
    let y =
      {
        Lens.get = (fun x -> x.y);
        Lens.set = (fun v -> fun x -> { x with y = v })
      }
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
