module type ORDERED =
sig
  type t

  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end

module OrderedInt =
struct
  type t = int

  let eq x y = x = y
  let lt x y = x < y
  let leq x y = eq x y || lt x y
end

