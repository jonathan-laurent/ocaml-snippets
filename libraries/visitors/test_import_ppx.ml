type constant =
  [%import: (Parsetree.constant[@with Location.t := (Location.t [@opaque])])]
[@@deriving visitors {variety= "iter"}]
