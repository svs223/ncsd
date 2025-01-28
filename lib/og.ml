let create_socket () = Unix.socket PF_INET SOCK_STREAM 0

let send sock ip data =
  let addr = Unix.ADDR_INET(Unix.inet_addr_of_string ip, 9223) in
  let () = Unix.connect sock addr in
  let len = Int32.of_int @@ String.length data in
  let len_b = Bytes.create 4 in
  Bytes.set_int32_be len_b 0 len;
  let _ = Unix.single_write sock len_b 0 4 in
  let _ = Unix.write_substring sock data 0 @@ String.length data in
  ()
  
let send_helper sock data ip = send sock ip data

let send_all sock data = 
  let f sock data _ ip = send sock data ip in
  Addrbook.apply (f sock data);
  ()

let create_server () =
  let sock = create_socket () in
  let rec serve () = 
    let msg = Outbox.take () in
    let data = Msg.to_json_string msg in
    let () = match Addrbook.ident msg.Msg.send_to with
    | Some ip -> send sock ip data
    | None -> send_all sock data
    in
    serve ()
  in serve

