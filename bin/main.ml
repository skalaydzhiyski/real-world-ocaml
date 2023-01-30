open Base

let rec remove_sequential_duplicates lst =
  match lst with
  | [] -> []
  | [x] -> [x]
  | first :: second :: tail ->
    if first = second then
      remove_sequential_duplicates (first :: tail)
    else
      first :: remove_sequential_duplicates (second :: tail)

let to_lowercase_extension filename =
  match String.rsplit2 filename ~on:'.' with
  | None -> filename
  | Some (left, right) ->
    left ^ "." ^ String.lowercase right

let lst = [1;2;3;4;5]
let lst2 = remove_sequential_duplicates lst
let both_lists_equal = List.equal (=) lst lst2
let lowercase_file_ext_res =
  List.map ~f:to_lowercase_extension ["something.TXT"; "another.txt"]

type point2d = { x: float; y: float }
let magnitude { x; y } =
  Float.sqrt (x **. 2. +. y **. 2.)
let distance p1 p2 =
  magnitude { x = p1.x -. p2.x; y = p1.y -. p2.y }

type circle_desc  = { center: point2d; radius: float }
type rect_desc    = { lower: point2d; width: float; height: float }
type segment_desc = { endpoint1: point2d; endpoint2: point2d }

type scene =
  | Circle of circle_desc
  | Rect of rect_desc
  | Segment of segment_desc

let is_inside_scene_element point scene_element =
  let open Float.O in
  match scene_element with
  | Circle { center; radius } ->
    distance center point < radius
  | Rect { lower; width; height } ->
       point.x > lower.x
    && point.x < lower.x + width
    && point.y > lower.y
    && point.y > lower.y + height
  | Segment _ -> false

let is_inside_scene point scene =
  List.exists ~f:(fun element -> is_inside_scene_element point element) scene

let numbers = [|1;2;3;4;5|]
let first = numbers.(0)
let update_second_element () = numbers.(1) <- 128

let sum_of_list nums =
  let sum = ref 0 in
  List.iter nums ~f:(fun current -> sum := !sum + current);
  !sum

let random_permutation array =
  let length = Array.length array in
  for left = 0 to length - 2 do
    let right = left + Random.int (length - left) in
    let temp = array.(left) in
    array.(left) <- array.(right);
    array.(right) <- temp
  done

let custom_sum_of_array array =
  let length = Array.length array in
  let current_idx = ref 0 in
  let sum = ref 0 in
  while !current_idx < length do
    sum := !sum + array.(!current_idx);
    current_idx := !current_idx + 1;
    Stdio.print_endline (Int.to_string !sum)
  done;
  !sum

let double_number_until upper_bound ~verbose =
  let number = ref 1 in
  while not (phys_equal !number upper_bound) do
    number := !number * 2;
    if verbose then
      Stdio.printf "Current number (%d/%d)..\n" !number upper_bound
  done;
  !number
              
let local_path =
  "/opt/local/bin:/opt/local/sbin:/Users/e1211913/.opam/default/bin:/Users/e1211913/.cabal/bin:/Users/e1211913/.ghcup/bin:/Users/e1211913/useful:/Users/e1211913/miniconda3/bin:/usr/local/sbin:/Users/e1211913/bin:/Users/e1211913/.local/bin:/Users/e1211913/.cabal/bin:/Users/e1211913/.ghcup/bin:/Users/e1211913/useful:/Users/e1211913/miniconda3/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/local/bin:/opt/local/sbin:/Users/e1211913/.cabal/bin:/Users/e1211913/.ghcup/bin:/Users/e1211913/useful:/Users/e1211913/miniconda3/bin:/usr/local/sbin:/Users/e1211913/miniconda3/condabin:/Users/e1211913/bin:/Users/e1211913/.local/bin:/Users/e1211913/.cargo/bin"

(* IMPORTANT -> OCAML has associatity of custom operators determined by the FIRST char in the operator *)
let (|>) x f = f x
let left_associative_chain path =
  String.split ~on:':' path
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:Stdio.print_endline

(* don't do this, we just needed an example *)
let (^>) x f = f x
let right_associative_chain path =
  (String.split ~on:':' path
   ^> List.dedup_and_sort ~compare:String.compare)
  ^> List.iter ~f:Stdio.print_endline

let three_functions f g h x = f @@ g @@ h x

let some_or_default default = function
  | None -> default
  | Some x -> x

let some_or_def_list lst =
  List.map ~f:(some_or_default 128) lst

let rec drop_from_list lst elem =
  match lst with
  | [] -> []
  | h :: t ->
    if h = elem then drop_from_list t elem
    else h :: drop_from_list t elem

(* fast *)
let add_one_fast_match x =
  match x with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | _ -> 128

(* slow *)
let add_one_slow_match x =
  if x = 0 then 1
  else if x = 1 then 2
  else if x = 2 then 3
  else if x = 3 then 4
  else if x = 4 then 5
  else 128

