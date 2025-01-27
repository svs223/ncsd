let _ =
  let open Ncs in
  let%lwt sock = Net.create_outgoing_socket () in
  let o_serve = Net.create_outgoing_server sock in
  Lwt_main.run @@ o_serve ()
