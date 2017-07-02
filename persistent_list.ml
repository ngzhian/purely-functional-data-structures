module type Stack =
sig
  type 'a stack
  val empty : 'a stack
  val is_empty : 'a stack -> bool

  val cons : 'a -> 'a stack -> 'a stack
  val head : 'a stack -> 'a
  val tail : 'a stack -> 'a stack
  val append : 'a stack -> 'a stack -> 'a stack
end

module ListStack : Stack =
struct
  type 'a stack = 'a list
  let empty = []
  let is_empty s = List.length s = 0

  let cons x s = x :: s
  let head s = List.hd s
  let tail s = List.tl s
  let rec append xs ys =
    if is_empty xs then ys
    else cons (head xs) (append (tail xs) ys)
end

module CustomStack : Stack =
struct
  type 'a stack = Nil | Cons of 'a * 'a stack

  let empty = Nil
  let is_empty = function
    | Nil -> true
    | _ -> false

  let cons x s = Cons (x, s)
  let head = function
    | Nil -> failwith "head on empty"
    | Cons (x, _) -> x
  let tail = function
    | Nil -> failwith "tail on empty"
    | Cons (_, s) -> s
  let rec append xs ys =
    if is_empty xs then ys
    else cons (head xs) (append (tail xs) ys)
end

(** Exercise 2.1
    O(n) time - walk down the list once
    O(n) space - no mutation, reuses the elements of the list
*)
let rec suffixes xs = match xs with
  | [] -> []
  | y::ys -> xs :: suffixes ys
