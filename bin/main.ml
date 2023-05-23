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

(** [TODO] continue with the following two items.
    1. example of memoisation for recursive functions.
    2. example of I/O using In_channel and Out_channel.

*)
