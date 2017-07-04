module type QUEUE = sig
  type 'a queue

  val empty : 'a queue
  val is_empty : 'a queue -> bool

  val snoc : 'a queue -> 'a -> 'a queue
  val head : 'a queue -> 'a
  val tail : 'a queue -> 'a queue
end

module BatchedQueue : QUEUE = struct
  (** Represented by two lists, a front and a rear
      The front contains front of the queue in correct order,
      the rear contains the rear of the queue in reverse order.
      The invariant is that if f is empty iff r is also empty.
      check_f ensures this invariant, and is called
      whenever f is changed.
   *)
  type 'a queue = 'a list * 'a list
  let empty = ([], [])
  let is_empty = function
    | [], _ -> true
    | _ -> false

  let check_f = function
    | [], r -> (List.rev r, [])
    | q -> q
  (** x is added to the back of the queue, thus the front of r.
      In case f is empty, we check_f to ensure invariant.
   *)
  let snoc (f, r) x = check_f (f, x::r)
  let head = function
    | [], _ -> failwith "head on empty"
    | (x::_, _) -> x
  (** f can become empty after we call tail, check_f to ensure invariant *)
  let tail = function
    | [], _ -> failwith "tail on empty"
    | (x::f, r) -> check_f (f, r)
end

module type DEQUE = sig
  type 'a queue

  val empty : 'a queue
  val is_empty : 'a queue -> bool

  val cons : 'a -> 'a queue -> 'a queue
  val head : 'a queue -> 'a
  val tail : 'a queue -> 'a queue

  val snoc : 'a queue -> 'a -> 'a queue
  val last : 'a queue -> 'a
  val init : 'a queue -> 'a queue
end

(** Exercise 5.1 (a) *)
module BatchedDeque : DEQUE = struct
  (** Represented by two lists, a front and a rear
      The front contains front of the queue in correct order,
      the rear contains the rear of the queue in reverse order.
      The invariant is that both f and r are to be non-empty
      whenever the deque contains two or more elements.
      check_f ensures this invariant, and is called
      whenever f is changed.
   *)
  type 'a queue = 'a list * 'a list
  let empty = ([], [])
  let is_empty = function
    | [], _ -> true
    | _ -> false

  let take n ls =
    let rec go n acc ls = match (n, ls) with
      | 0, _ | _, [] -> acc, ls
      | n, x::xs -> go (n-1) (x::acc) xs
    in
    let f, r = go n [] ls in
    List.rev r, ls

  let check_f = function
    | [], r ->
      let f, r = take (List.length r / 2) r in
      (f, List.rev r)
    | f, [] ->
      let f, r = take (List.length f / 2) f in
      (f, List.rev r)
    | q -> q
  let cons x (f, r) = check_f (x::f, r)
  let head = function
    | [], _ -> failwith "head on empty"
    | (x::_, _) -> x
  let tail = function
    | [], _ -> failwith "tail on empty"
    | (x::f, r) -> check_f (f, r)
  let snoc (f, r) x = check_f (f, x::r)
  let last = function
    | (_, []) -> failwith "last on empty"
    | (_, x::_) -> x
  let init = function
    | _, [] -> failwith "init on empty"
    | f, x::r -> check_f (f, r)
  (** Exercise 5.1 (b)
      Each operation takes amortized cost of O(1)
      cons takes 1 step, increase f, and thus potential by 1, csot 2
      tail takes 1 step, if does not reverse, cost 1
      tail takes (r/2) + 1 step if reverses, reduce potential by (r/2), cost 1
      snoc takes 1 step, increase r, decrease potential by 1, cost 0
      init takes 1, if does not reverse, cost 1
      init takes (r/2) + 1 step if reverses, reduce potential by (r/2), cost 1
   *)
end
