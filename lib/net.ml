let backlog = 16
let address = Unix.inet_addr_of_string "0.0.0.0"
let port = 2239

let handle_msg req =
  try
    Logs.info (fun m -> m "Handling Message");
    let msg = Msg.of_json_string req in
    let st = msg.Msg.raddr in
    match msg.Msg.typ with
    | "ping" ->
        Logs.info (fun m -> m "Ping");
        Some
          (Msg.to_json_string
          @@ Msg.packet
               ~raddr:Domain.(self.address)
               ~sendto:st ~typ:"pong" ~payload:"10.0.0.193")
    | "pong" ->
        Logs.info (fun m -> m "Pong");
        Addrbook.learn (msg.Msg.raddr, msg.Msg.payload);
        None
    | "pass" ->
        Logs.info (fun m -> m "Pass");
        Outbox.pass msg;
        None
    | _ ->
        Logs.info (fun m -> m "unknown");
        Some
          (Msg.to_json_string
          @@ Msg.packet
               ~raddr:Domain.(self.address)
               ~sendto:st ~typ:"error" ~payload:"Invalid Type")
  with _ ->
    Logs.info (fun m -> m "malformed");
    Some
      (Msg.to_json_string
      @@ Msg.packet
           ~raddr:Domain.(self.address)
           ~sendto:"rts" ~typ:"error" ~payload:"Malformed Request")

let handle_connection ic oc =
  let%lwt data = Lwt_io.read_line_opt ic in
  match data with
  | Some msg -> (
      try%lwt
        let%lwt () = Lwt_io.write oc msg in
        Lwt.return ()
      with e ->
        Logs.err (fun m -> m "%s" @@ Printexc.to_string e);
        Lwt.fail_with "write")
  | None -> Lwt.return ()

let run_conn (conn, _) =
  let ic = Lwt_io.(of_fd ~mode:Input conn) in
  let oc = Lwt_io.(of_fd ~mode:Output conn) in
  Lwt.return @@ handle_connection ic oc

let create_outgoing_server sock =
  let rec serve proms () =
    let%lwt allconns, _ = Lwt_unix.accept_n sock 10 in
    let promises =
      List.map
        (run_conn)
        allconns
    in
    serve (proms @ promises) ()
  in
  serve []

let create_outgoing_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let%lwt () = bind sock @@ ADDR_INET(address, 92239) in
  listen sock backlog;
  Lwt.return sock
