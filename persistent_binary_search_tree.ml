module type SET =
sig
  type elem
  type set

  val empty : set
  val insert : elem -> set -> set
  val member : elem -> set -> bool
end

module UnbalancedSet = functor (Element : Ordered.ORDERED) -> struct
  type elem = Element.t
  type tree = E | T of tree * elem * tree
  type set = tree

  let empty = E
  (** Exercise 2.2
      Simple implementation of member will lead to worst case 2d comparisons,
      when searching for the right most leaf (max element) or an element
      that is bigger than everything in the set.
  *)
  let rec member x s = match s with
    | E -> false
    | T (l, y, r) when Element.lt x y -> member x l
    | T (l, y, r) when Element.lt y x -> member x r
    | _ -> true

  (** Exercise 2.2
      This implementation keeps track of a candidate that could be what
      we are searching for.
      Just before we go into the right branch, we set the current node as the candidate.
      When we reach the empty node we compare the candidate to our search term.
      If the node is what we are looking for, we will set it to our candidate,
      and continue into the right subtree. However since all nodes in the right
      subtree is bigger, we will never go into another right subtree.
      So candidate is set to where we first make a right turn.
      The exposition is found in [And91].
  *)
  let member2 x s =
    let rec candidate x s c = match s with
      | E -> c
      | T (l, y, r) when Element.lt x y -> candidate x l c
      | T (l, y, r) -> candidate x r (Some y)
    in
    candidate x s None = Some x

  let rec insert x s = match s with
    | E -> T (empty, x, empty)
    | T (l, y, r) when Element.lt x y -> T (insert x l, y, r)
    | T (l, y, r) when Element.lt y x -> T (l, y, insert x r)
    | _ -> s

  (** Exercise 2.3, Exercise 2.4
      The simple version of insert copies the entire search path if
      an existing element is inserted into the tree.
      This new version first checks if x already exists using the member2
      function that takes d+1 comparisons and does no unnecessary copying.
   *)
  let rec insert2 x s =
    if member2 x s then s
    else begin
      match s with
      | E -> T (empty, x, empty)
      | T (l, y, r) when Element.lt x y -> T (insert2 x l, y, r)
      | T (l, y, r) when Element.lt y x -> T (l, y, insert2 x r)
      | _ -> s
    end

  (** Exercise 2.5 (a)
      Build a complete tree of depth d.
      O(d) time because the recursion takes d steps.
  *)
  let rec complete x d = match d with
    | 0 -> E
    | d -> let sub = complete x (d - 1) in T (sub, x, sub)

  (** Exercise 2.5 (b)
      Creates a balanced binary tree of size n.
  *)
  let rec create x n = match n with
    | 0 -> E
    | n when (n - 1) mod 2 = 0 ->
        let sub = create x ((n-1)/2) in T (sub, x, sub)
    | _ -> let (l, r) = create2 x ((n-1)/2) in T (l, x, r)
  and create2 x m = (create x m, create x (m + 1))
end

module IntTree = UnbalancedSet(Ordered.OrderedInt)

let test_tree =
  IntTree.T (
    IntTree.T (
      IntTree.T (IntTree.E, 2, IntTree.E),
      3,
      IntTree.T (IntTree.E, 4, IntTree.E)
    ),
    5,
    IntTree.T (
      IntTree.T (IntTree.E, 6, IntTree.E),
      7,
      IntTree.T (IntTree.E, 8, IntTree.E)
    )
  )

(** References
    [And91] A note on searching in a binary search tree, Arne Andersson
*)
