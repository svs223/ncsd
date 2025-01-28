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
          (Msg.packet
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
          (Msg.packet
               ~raddr:Domain.(self.address)
               ~sendto:st ~typ:"error" ~payload:"Invalid Type")
  with _ ->
    Logs.info (fun m -> m "malformed");
    Some
      (Msg.packet
           ~raddr:Domain.(self.address)
           ~sendto:"rts" ~typ:"error" ~payload:"Malformed Request")

let handle_conn fd =
  Logs.info (fun m -> m "Handling connection");
  let size_b = Bytes.create 4 in
  let _ = Unix.read fd size_b 0 4 in
  let size = Int32.to_int @@ Bytes.get_int32_be size_b 0 in
  let data_b = Bytes.create size in
  let _ = Unix.read fd data_b 0 size in
  
  let data = if Bytes.is_valid_utf_8 data_b
  then Some (Bytes.to_string data_b)
  else None 
  in
  
  match data with
  | Some req -> (
    let resp = handle_msg req in
    match resp with
    | Some resp -> Outbox.pass resp; fd
    | None -> fd)
  | None -> failwith "invalid encoding"

      
let run_conn fd =
  let sock, _ip = Unix.accept fd in
  sock

let create_socket () =
  let open Unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock @@ ADDR_INET(address, 2239);
  listen sock backlog;
  sock

let create_server () =
  let sock = create_socket () in
  let rec serve r w _ () =
    let (ri, wi, _) = Unix.select r w [] 60. in
    let wn = List.map (run_conn) ri in
    let we = List.map (handle_conn) wi in
    serve r (we @ wn) [] ()
  in
  serve [sock] [] []

