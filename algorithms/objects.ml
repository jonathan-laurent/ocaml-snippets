class point x y =
  object
    method x: int = x
    method y: int = y
  end

let p = new point 3 5

let p: <x: int; y: int> = new point 3 5

let p: point = object method x = 3 method y = 5 end

(* Error! *)
(* let p: point = object method x = 3 method y = 5 method z = 5 end *)

let p: #point = object method x = 3 method y = 5 method z = 5 end

let p: <x: int; y: int; ..> = object method x = 3 method y = 5 method z = 5 end