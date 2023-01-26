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

type circle_desc = { center: point2d; radius: float }
type rect_desc = { lower: point2d; width: float; height: float}
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

let sum nums =
  let sum = ref 0 in
  List.iter nums ~f:(fun current -> sum := !sum + current);
  !sum
