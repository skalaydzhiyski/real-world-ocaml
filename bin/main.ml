open Core

(** IO stdin *)
let () =
  printf "Pick a timezone: %!"; (* `%!` is required in the format string to flush the buffer *)
  match In_channel.(input_line stdin) with
  | None -> failwith "No timezone provided"
  | Some timezone ->
    let zone = Time_unix.Zone.find_exn timezone in
    let time_string = Time.to_string_abs (Time.now()) ~zone in
    printf "The time in %s is %s. \n%!" time_string timezone

(** IO files *)
