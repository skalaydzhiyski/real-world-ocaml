open Core
open Re

type service_info = {
  service_name : string;
  port         : int;
  protocol     : string;
}

let pat = "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)"

let service_info_of_string line =
  let matches = Re.exec (Re.Posix.compile_pat pat) line in
  { service_name = Re.Group.get matches 1;
    port = Int.of_string @@ Re.Group.get matches 2;
    protocol = Re.Group.get matches 3;
  }

let ssh_services services_path =
  let lines =
    List.filter
      ~f:(fun l -> String.compare (String.prefix l 3) "ssh" == 0)
    @@ In_channel.read_lines services_path
  in
    List.map ~f:service_info_of_string lines
  
  
