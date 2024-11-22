open Define

let get_es_version defines =
	try int_of_string (Define.defined_value defines Define.JsEs) with _ -> 0

let map_source_header defines f =
	match Define.defined_value_safe defines Define.SourceHeader with
	| "" -> ()
	| s -> f s