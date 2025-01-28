let f_exists path = 
  match Unix.stat path with
  | exception _ -> false
  | _ -> true

let exists path = 
  if f_exists path then Some path
  else None

let first paths = List.find_opt (f_exists) paths
