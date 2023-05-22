open Base
open Stdio

type basic_color =
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

let basic_color_to_int = function
  | Black -> 0 | Red -> 1 | Green -> 2 | Yellow -> 3 | Blue -> 4 | Magenta -> 5 | Cyan -> 6 | White -> 7

type weight = Regular | Bold
type color =
  | Basic of basic_color * weight
  | RGB of int * int * int
  | Greyscale of int

let color_by_number text color_number =
  Printf.sprintf "\027[38;5;%dm%s\027[0m" color_number text

let print_color_message =
  let red = color_by_number "Red" (basic_color_to_int Red) in
  printf "Hello %s World\n" red

let color_to_int = function
  | Basic (bcolor, weight) ->
    let base =
      match weight with
      | Bold -> 8
      | Regular -> 0
    in
    base + basic_color_to_int bcolor
  | RGB (r,g,b) -> 16 + b + g*6 + r * 36
  | Greyscale value -> 232 + value

let color_print color s =
  printf "%s\n" (color_by_number s @@ color_to_int color)


module Time_ns = Core.Time_ns

module Common = struct
  type t =
    { session_id: string;
      time: Time_ns.t;
    }
  [@@deriving fields]
end

module Log_entry = struct
  type t =
    { important: bool;
      message: string;
    }
    [@@deriving fields]
end

module Heartbeat = struct
  type t = { status_message: string }
    [@@deriving fields]
end
  
module Logon = struct
  type t =
    { user: string;
      credentials: string;
    }
    [@@deriving fields]
end

type message_details = | Logon of Logon.t
                       | Heartbeat of Heartbeat.t
                       | Log_entry of Log_entry.t

let user_messages user (messages : (Common.t * message_details) list) =
  let (result, _) =
    List.fold messages
      ~init:([], Set.empty (module String))
      ~f:(
        fun ((user_messages, user_sessions) as acc) ((common, details) as msg) ->
          match details with 
          | Logon m ->
            if String.(m.user = user) then
              (** append the message to user messages and add session_id to the set of sessions *)
              (msg :: user_messages, Set.add user_sessions common.session_id)
            else
              (** do nothing and return the original accumulator *)
              acc
          | Heartbeat _ | Log_entry _ ->
            if Set.mem user_sessions common.session_id then
              (msg :: user_messages, user_sessions)
            else
              acc
       )
  in
  List.rev result
