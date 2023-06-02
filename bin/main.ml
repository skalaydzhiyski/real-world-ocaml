open Core

(** IO stdin *)
let print_timezone_msg () =
  printf "Pick a timezone: %!"; (* `%!` is required in the format string to flush the buffer *)
  match In_channel.(input_line stdin) with
  | None -> failwith "No timezone provided"
  | Some timezone ->
    let zone = Time_unix.Zone.find_exn timezone in
    let time_string = Time.to_string_abs (Time.now()) ~zone in
    printf "The time in %s is %s. \n%!" time_string timezone

(** IO files *)
let create_number_file filename numbers =
  let outc = Out_channel.create filename in
  List.iter numbers
    ~f:(fun x -> Out_channel.fprintf outc "%d\n" x);
  Out_channel.close outc

(** [read_lines] here reads the full file *)
let sum_file_numbers filename =
  List.fold ~init:0 ~f:(fun a x -> Int.of_string x + a)
    @@ In_channel.read_lines filename

(** [with_file + fold_lines] is a better approach in case we have bigger data *)
let sum_file_better filename =
  In_channel.with_file filename
    ~f:(fun file ->
        In_channel.fold_lines file
          ~init:0
          ~f:(fun a x -> a + Int.of_string x))
