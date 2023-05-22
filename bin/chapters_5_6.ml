open Core
open Re

type service_info = {
  service_name : string;
  port         : int;
  protocol     : string;
  comment      : string option;
}

let service_info_of_string input_line =
  let pat = "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)" in
  let (line, comment) =
    match String.rsplit2 ~on:'#' input_line with
    | None -> (input_line, None)
    | Some (left, right) -> (left, Some right)
  in
  let matches = Re.exec (Re.Posix.compile_pat pat) line in
  let service_name = Re.Group.get matches 1 in
  let port = Int.of_string @@ Re.Group.get matches 2 in
  let protocol = Re.Group.get matches 3 in
  { service_name; port; protocol; comment }

let ssh_services services_path =
  let lines =
    List.filter
      ~f:(fun l -> String.compare (String.prefix l 3) "ssh" = 0 )
    @@ In_channel.read_lines services_path
  in
    List.map ~f:service_info_of_string lines
  
let service_info_to_string
  { service_name; port; protocol; comment}
  =
  let comment =
    match comment with
    | None -> ""
    | Some x -> x
  in
  sprintf "%s %i %s %s" service_name port protocol comment

let create_service_info ~service_name ~port ~protocol comment =
  { service_name; port; protocol; comment }

module Logon = struct
  type t = {
    session_id: string;
    time: Time_ns.t;
    user: string;
    credentials: string;
  }
  [@@deriving fields]
end

let show_field_generic field to_string record =
  let name = Field.name field in
  let field_string = to_string (Field.get field record) in
  name ^ ": " ^ field_string
