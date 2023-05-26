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
  let memo_table = Hashtbl.create m in
  let f' = (fun x -> Hashtbl.find_or_add memo_table x ~default:(fun () -> f x)) in
  f' (** [memo_table] gets collected only when [f'] gets collected! *)


(** corecursion *)
let make_rec f_norec =
  let rec f x = f_norec f x in
  f

let fib_norec fib x =
  if Int.(x <= 1) then x
  else fib (x-1) + fib (x-2)

let fib = make_rec fib_norec

(** corecursion + memoisation *)
let memo_rec m f_norec x =
  let fref = ref (fun _ -> assert false) in
  let f x = f_norec !fref x in
  let fmem = memoize m f in
  fref := fmem;
  fmem x

(** [TODO] Continue here and analyse the memoisation behaviour of fibonacci. *)
