let server_wait (i_thread, o_thread) =
  Thread.join i_thread;
  Thread.join o_thread;
  ()

let start_threaded_server () =
  let i_serve = Ig.create_server () in
  let o_serve = Og.create_server () in
  let i_thread = Thread.create i_serve () in
  let o_thread = Thread.create o_serve () in
  (i_thread, o_thread)

