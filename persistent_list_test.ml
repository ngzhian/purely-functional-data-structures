let list_gen =
  let module L = Persistent_list.ListStack in
  let cons a b = L.cons a b in
  QCheck.Gen.(sized @@ fix
  (fun self n -> match n with
    | 0 -> map (fun x -> L.cons x L.empty) nat
    | n ->
      frequency
        [1, map (fun x -> L.cons x L.empty) nat;
         2, map (cons n) (self (n/2))]
  ));;

let arbitrary_list =
  QCheck.make list_gen;;

let cons_head_tail_same =
  let module L = Persistent_list.ListStack in
  (fun (l:int L.stack) -> L.cons (L.head l) (L.tail l) = l)

let test_cons_head_tail_same =
  QCheck.Test.make ~count:100 ~name:"test1"
    arbitrary_list
    cons_head_tail_same
;;

QCheck_runner.run_tests [test_cons_head_tail_same];;
