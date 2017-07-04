module type HEAP =
sig
  type elem
  type heap

  val empty : heap
  val is_empty: heap -> bool

  val insert : elem -> heap -> heap
  val merge : heap -> heap -> heap

  val find_min : heap -> elem
  val delete_min : heap -> heap
end

(** Exercise 3.1
    Prove that right spine of leftist heap of size n contains at most
    floor(log(n + 1)) elements.

    1. You get the most number of elements in the right spine when
    rank of left and right child of root is the same.

    for rank = 0, n >= 1
    for rank = 1, n >= 3
    for rank = 2, n >= 7

    n >= 2 ^ (rank + 1) -1
    n + 1 >= 2 ^ (rank + 1)
    log (n + 1) >= rank + 1
    rank <= log (n + 1) - 1

    Reference: http://www.dgp.toronto.edu/people/JamesStewart/378notes/10leftist/
*)
module LeftistHeap = functor (Element : Ordered.ORDERED) -> (struct
    type heap =
      | E
        (* binary tree with rank information *)
      | T of int * Element.t * heap * heap
    type elem = Element.t

    let empty = E
    let is_empty = function
      | E -> true
      | _ -> false

    let rank = function
      | E -> 0
      | T (r, _, _, _) -> r
    let make_t (x, a, b) =
      if rank a >= rank b
      then T (rank b + 1, x, a, b)
      else T (rank a + 1, x, b, a)
    let rec merge h1 h2 = match (h1, h2) with
      | h, E | E, h -> h
      | T (_, x, a1, b1), T (_, y, a2, b2) ->
        if Element.leq x y
        then make_t (x, a1, merge b1 h2)
        else make_t (y, a2, merge h1 b2)

    let singleton x = T (1, x, E, E)
    let insert x h = merge (singleton x) h
    (** Exercise 3.2 *)
    let rec insert2 x = function
      | E -> T (1, x, E, E)
      | T (_, y, a, b) ->
        if Element.leq x y
        then make_t (x, a, insert2 y b)
        else make_t (y, a, insert2 x b)
    let find_min = function
      | E -> failwith "find_min on E"
      | T (_, x, _, _) -> x
    let delete_min = function
      | E -> failwith "delete_min on E"
      | T (_, _, a, b) -> merge a b

    let from_list =
      List.fold_left (fun a b -> merge a (singleton b)) E
    let from_list2 xs =
      let xs = List.map singleton xs in
      let rec fl xs = match xs with
      | [] -> []
      | x::[] -> [x]
      | x1::x2::xs -> merge x1 x2 :: fl xs
      in
      match fl xs with
      | x::[] -> x
      | _ -> failwith "from_list2 fail"

    let rec pair_up = function
      | [] -> [], None
      | x::[] -> [], Some x
      | x::y::tl -> let p, r = pair_up tl in (x, y)::p, r
    let rec from_list3 xs =
      (* from pair of heap to heap *)
      let rec pair_to_heap (x, y) = merge x y in
      let xs = List.map singleton xs in
      let rec fl xs = match xs with
        | [] -> E
        | x::[] -> x
        | xs ->
          begin
            let pairs, r = pair_up xs in
            let h = List.map pair_to_heap pairs in
            match r with
            | None -> fl h
            | Some x -> fl (x::h)
          end
      in
      fl xs

end : HEAP)

(** Exercise 3.4 *)
module WeightBasedLeftistHeap = functor (Element : Ordered.ORDERED) -> (struct
    type heap =
      | E
        (* binary tree with rank information *)
      | T of int * Element.t * heap * heap
    type elem = Element.t

    let empty = E
    let is_empty = function
      | E -> true
      | _ -> false

    let rank = function
      | E -> 0
      | T (r, _, _, _) -> r
    let make_t (x, a, b) =
      if rank a >= rank b
      then T (rank a + rank b + 1, x, a, b)
      else T (rank a + rank b + 1, x, b, a)
    (** Exercise 3.4 c
        Need to change this to a single top down pass
     *)
    let rec merge h1 h2 = match (h1, h2) with
      | h, E | E, h -> h
      | T (_, x, a1, b1), T (_, y, a2, b2) ->
        if Element.leq x y
        then
          begin
            let r = merge b1 h2 in
            if rank a1 >= rank r
            then T (rank r + rank a1 + 1, x, a1, r)
            else T (rank r + rank a1 + 1, x, r, a1)
          end
        else
          begin
            let r = merge b2 h1 in
            if rank a2 >= rank r
            then T (rank r + rank a2 + 1, y, a2, r)
            else T (rank r + rank a2 + 1, y, r, a2)
          end

    let singleton x = T (1, x, E, E)
    let insert x h = merge (singleton x) h
    let find_min = function
      | E -> failwith "find_min on E"
      | T (_, x, _, _) -> x
    let delete_min = function
      | E -> failwith "delete_min on E"
      | T (_, _, a, b) -> merge a b

    let from_list =
      List.fold_left (fun a b -> merge a (singleton b)) E
    let from_list2 xs =
      let xs = List.map singleton xs in
      let rec fl xs = match xs with
      | [] -> []
      | x::[] -> [x]
      | x1::x2::xs -> merge x1 x2 :: fl xs
      in
      match fl xs with
      | x::[] -> x
      | _ -> failwith "from_list2 fail"

end )

module IntHeap = WeightBasedLeftistHeap(Ordered.OrderedInt)
