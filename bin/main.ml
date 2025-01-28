let _ =
  let open Ncs in
  Main.setup_logs ();
  let threads = Net.start_threaded_server () in
  Addrbook.learn "NCS-XYZ" "10.0.0.141"
  Outbox.pass @@ Msg.packet ~raddr:"NCS-ABC" ~sendto:"NCS-XYZ" ~typ:"ping" ~payload:"10.0.0.141";
  Net.server_wait threads
