let get_name () =
  let rec loop acc i =
    match i with
    | 0 -> acc
    | _ ->
        let chr = Random.int_in_range ~min:65 ~max:90 |> Stdlib.char_of_int in
        loop (acc @ [ chr ]) (i - 1)
  in

  let name = loop [] 32 in
  let name' = List.map (String.make 1) name in
  "NCS-" ^ List.fold_left ( ^ ) "" name'
