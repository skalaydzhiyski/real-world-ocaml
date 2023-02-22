open Base

let id = fun x -> x

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
              
let local_path = "something:somethingelse:somethingelseevenmore"

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

(* ------------------ Lists -------------------------- *)

let header = ["language"; "architect"; "first_release"]
let rows   = [
  ["Lisp"; "John McCarthy"; "1958"];
  ["C"; "Denis Ritchie"; "1969"];
  ["OCaml"; "Xavier Leroy"; "1996"];
  ["Haskell"; "Simon Peyton Jones"; "1990"]
]

let max_widths header rows =
  let lengths lst = List.map ~f:String.length lst in
  List.fold
    ~f:(fun ac row -> List.map2_exn ~f:Int.max ac (lengths row))
    ~init:(lengths header)
    rows

let render_separator widths =
  let start_ = "|-" in
  let end_   = "-|" in
  let pieces = List.map ~f:(fun w -> String.make w '-') widths in
  start_ ^ String.concat ~sep:"-+-" pieces ^ end_

let pad s length =
  s ^ String.make (length - String.length s) ' '

let render_row row ~widths =
  let start_ = "| " in
  let end_   = " |" in
  let padded = List.map2_exn ~f:pad row widths in
  start_ ^ String.concat ~sep:" | " padded ^ end_

let render_table header rows =
  let widths = max_widths header rows in
  let rendered_data = List.map ~f:(render_row ~widths) rows in
  let separator = render_separator widths in
  String.concat ~sep:"\n" (
    separator
    :: render_row header ~widths
    :: separator
    :: rendered_data @ [separator]
  )

let filter_map ~fcond ~f lst =
  List.filter_map ~f:(fun x -> if fcond x then Some (f x) else None) lst

let filter_map_use =
  filter_map ~fcond:(fun x -> x % 2 <> 0) ~f:(fun x -> x ** 2) [1;2;3;4;5]
      
let map_to_first_partition ~fcond ~f lst  =
  let open Core in
  let pmf = fun x -> if fcond x then First (f x) else Second x in
  let (first, _) = List.partition_map ~f:pmf lst in
  first

let map_to_first_partition_use =
  map_to_first_partition
    ~fcond:(fun x -> x % 2 <> 0) ~f:(fun x -> x ** 2)
    [1;2;3;4;5]

let custom_concat_use =
  List.fold
    ~init:[]
    ~f:(fun ac current -> ac @ current)
    [[1;2];[3;4];[5;6]]

let concat_map_use =
  List.concat_map
    ~f:(fun x -> [List.length x ** 2])
    [[1;2];[3;4];[5;6]]


(* tail recursion *)
let rec length_non_tail = function
  | [] -> 0
  | _ :: tl -> 1 + length_non_tail tl

let rec length_tail lst acc =
  match lst with
  | [] -> acc
  | _ :: tl -> length_tail tl (acc + 1)


let rec sum_evens_non_tail = function
  | [] -> 0
  | hd :: tl ->
    if hd % 2 = 0 then hd + sum_evens_non_tail tl
    else sum_evens_non_tail tl

let rec sum_evens_tail lst acc =
  match lst with
  | [] -> acc
  | hd :: tl ->
    sum_evens_tail tl
    @@ acc + (if hd % 2 = 0 then hd else 0)
  
let rec remove_sequential_duplicates_better lst =
  match lst with
  | [] | [_] as l -> l
  | first :: (second :: _ as tail) ->
    if first = second then
      remove_sequential_duplicates_better tail
    else
      first :: remove_sequential_duplicates_better tail
        
  
