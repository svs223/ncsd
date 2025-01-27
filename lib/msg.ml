open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  raddr : string; [@key "return_address"]
  send_to : string; [@key "to"]
  typ : string; [@key "type"]
  proto : string; [@key "protocol"]
  payload : string;
  signature : string;
  ttl : int64;
  id : int64;
}
[@@deriving yojson, yojson_fields]

let to_json_string t = Yojson.Safe.to_string (yojson_of_t t)
let of_json_string s = t_of_yojson (Yojson.Safe.from_string s)

let packet ~raddr ~sendto ~typ ~payload =
  {
    raddr;
    send_to = sendto;
    typ;
    proto = "CT";
    payload;
    signature = "";
    ttl = 5L;
    id = Random.int64_in_range ~min:1_000_000L ~max:9_999_999L;
  }
