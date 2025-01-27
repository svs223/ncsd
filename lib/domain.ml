open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  maj_version : int;
  min_version : int;
  proto_revision : int;
  address : string;
  agent : string;
}
[@@deriving yojson, yojson_fields]

let to_json_string t = Yojson.Safe.to_string (yojson_of_t t)
let of_json_string s = t_of_yojson (Yojson.Safe.from_string s)

let self =
  Random.self_init ();
  let name = Auth.get_name () in
  {
    maj_version = 0;
    min_version = 1;
    proto_revision = 1;
    address = name;
    agent = "libncs";
  }
