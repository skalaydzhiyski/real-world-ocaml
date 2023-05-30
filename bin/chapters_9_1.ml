open Core

let test_for_loop () =
  for number = 0 to 10 do
    print_endline @@ Int.to_string number
  done

let test_while_loop () =
  let number = ref 0 in
  while Int.(!number <> 10) do
    print_endline @@ Int.to_string !number;
    number := !number + 1
  done

let test_iter () =
  List.iter ~f:(fun x -> print_endline @@ Int.to_string x) [1;2;3];;

let some_array = [|1;2;3;4;5|]
let test_some_array_funcs () =
  let index = 3 in
  let value = some_array.(index) in
  print_endline @@ Int.to_string value

(** memoisation *)
let memoize m f =
  print_endline "check cache table";
  let memo_table = Hashtbl.create m in
  let f' = (fun x -> Hashtbl.find_or_add memo_table x ~default:(fun () -> f x)) in
  f' (** [memo_table] gets collected only when [f'] gets collected! *)

(** corecursion *)
let fib_norec fib_rec x =
  if Int.(x <= 1) then 1
  else fib_rec (x-1) + fib_rec (x-2)

let rec fib_rec x = fib_norec fib_rec x

(** Here the memoised function is [fib_rec], this means
    the first time we call [fib_bad_memo] - we get a slow computation
    and only then we start to do table lookups in the cache.
*)
let fib_bad_memo = memoize (module Int) fib_rec

(** Here we try to hack the recursive call by
    1. memoising fib_rec (this function can be used with `let rec`,
                          because the compilerd has no idea what sits behind the ref)
    2. overwriting [fref] with the memoised version of [fmem],
       thus making sure every recursive call gets through the cache table before doing compute.
*)
let fib_memo x =
  let fref = ref (fun _ -> assert false) in
  let rec fib_rec x = fib_norec !fref x in
  let fmemo = memoize (module Int) fib_rec in
  fref := fmemo;
  fmemo x

(** Here we try to replace the mutating behaviour with laziness,
    by making the recursive call lazy
*)
let fib_lazy_memo x =
  let rec fib = lazy (
    memoize (module Int) (fun x -> fib_norec (force fib) x)
  ) in
  (force fib) x

let f = lazy (fun x -> x + 1)
let run_func f x = f x
let res = run_func (force f) 10
