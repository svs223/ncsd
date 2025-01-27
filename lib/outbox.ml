module Sem = Semaphore.Counting

let outbox : Msg.t Queue.t = Queue.create ()
let out_mt = Mutex.create ()
let out_cond = Sem.make 0

let take () =
  Sem.acquire out_cond;
  Mutex.protect out_mt (fun () ->
      let x =
        try Queue.peek outbox with Queue.Empty -> failwith "Empty Queue"
      in
      Queue.pop outbox |> ignore;
      x)

let pass msg =
  Sem.release out_cond;
  Mutex.protect out_mt (fun () -> Queue.push msg outbox);
  ()
