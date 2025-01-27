let known : (string, string) Hashtbl.t = Hashtbl.create ~random:true 32
let apply f = Hashtbl.iter f known

let learn addr =
  let ncs, ip = addr in
  Hashtbl.replace known ncs ip

let forget addr =
  let ncs, _ = addr in
  Hashtbl.remove known ncs

let ident ncs = Hashtbl.find_opt known ncs
let ident_exn ncs = Hashtbl.find known ncs
