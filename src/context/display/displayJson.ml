open Globals
open JsonRpc
open Jsonrpc_handler
open Json
open Common
open DisplayTypes.DisplayMode
open Timer
open Genjson
open Type
open DisplayProcessingGlobals

(* Generate the JSON of our times. *)
let json_of_times root =
	let rec loop node =
		if node == root || node.time > 0.0009 then begin
			let children = ExtList.List.filter_map loop node.children in
			let fl = [
				"name",jstring node.name;
				"path",jstring node.path;
				"info",jstring node.info;
				"time",jfloat node.time;
				"calls",jint node.num_calls;
				"percentTotal",jfloat (if root.time = 0. then 0. else (node.time *. 100. /. root.time));
				"percentParent",jfloat (if node == root || node.parent.time = 0. then 0. else node.time *. 100. /. node.parent.time);
			] in
			let fl = match children with
				| [] -> fl
				| _ -> ("children",jarray children) :: fl
			in
			Some (jobject fl)
		end else
			None
	in
	loop root

let supports_resolve = ref false

let create_json_context jsonrpc may_resolve =
	Genjson.create_context ~jsonrpc:jsonrpc (if may_resolve && !supports_resolve then GMMinimum else GMFull)

let send_string j =
	raise (Completion j)

let send_json json =
	send_string (string_of_json json)

class display_handler (jsonrpc : jsonrpc_handler) com (cs : CompilationCache.t) = object(self)
	val cs = cs;

	method get_cs = cs

	method enable_display ?(skip_define=false) mode =
		com.display <- create mode;
		Parser.display_mode := mode;
		if not skip_define then Common.define_value com Define.Display "1"

	method set_display_file was_auto_triggered requires_offset =
		let file = jsonrpc#get_opt_param (fun () ->
			let file = jsonrpc#get_string_param "file" in
			Path.get_full_path file
		) file_input_marker in
		let contents = jsonrpc#get_opt_param (fun () ->
			let s = jsonrpc#get_string_param "contents" in
			Some s
		) None in

		let pos = if requires_offset then jsonrpc#get_int_param "offset" else (-1) in
		Parser.was_auto_triggered := was_auto_triggered;

		if file <> file_input_marker then begin
			let file_unique = com.file_keys#get file in

			DisplayPosition.display_position#set {
				pfile = file;
				pmin = pos;
				pmax = pos;
			};

			com.file_contents <- [file_unique, contents];
		end else begin
			let file_contents = jsonrpc#get_opt_param (fun () ->
				jsonrpc#get_opt_param (fun () -> jsonrpc#get_array_param "fileContents") []
			) [] in

			let file_contents = List.map (fun fc -> match fc with
				| JObject fl ->
					let file = jsonrpc#get_string_field "fileContents" "file" fl in
					let file = Path.get_full_path file in
					let file_unique = com.file_keys#get file in
					let contents = jsonrpc#get_opt_param (fun () ->
						let s = jsonrpc#get_string_field "fileContents" "contents" fl in
						Some s
					) None in
					(file_unique, contents)
				| _ -> invalid_arg "fileContents"
			) file_contents in

			let files = (List.map (fun (k, _) -> k) file_contents) in
			com.file_contents <- file_contents;

			match files with
			| [] -> DisplayPosition.display_position#set { pfile = file; pmin = pos; pmax = pos; };
			| _ -> DisplayPosition.display_position#set_files files;
		end
end

class hxb_reader_api_com
	~(minimal_restore : bool)
	(com : Common.context)
	(cc : CompilationCache.context_cache)
= object(self)
	method make_module (path : path) (file : string) =
		let mc = cc#get_hxb_module path in
		{
			m_id = mc.mc_id;
			m_path = path;
			m_types = [];
			m_statics = None;
			(* Creating a new m_extra because if we keep the same reference, display requests *)
			(* can alter it with bad data (for example adding dependencies that are not cached) *)
			m_extra = { mc.mc_extra with m_deps = mc.mc_extra.m_deps }
		}

	method add_module (m : module_def) =
		com.module_lut#add m.m_path m;

	method resolve_type (pack : string list) (mname : string) (tname : string) =
		let path = (pack,mname) in
		let m = self#find_module path in
		List.find (fun t -> snd (t_path t) = tname) m.m_types

	method resolve_module (path : path) =
		self#find_module path

	method find_module (m_path : path) =
		try
			com.module_lut#find m_path
		with Not_found -> try
			cc#find_module m_path
		with Not_found ->
			let mc = cc#get_hxb_module m_path in
			let reader = new HxbReader.hxb_reader mc.mc_path com.hxb_reader_stats (Some cc#get_string_pool_arr) (Common.defined com Define.HxbTimes) in
			fst (reader#read_chunks_until (self :> HxbReaderApi.hxb_reader_api) mc.mc_chunks (if minimal_restore then MTF else EOM) minimal_restore)

	method basic_types =
		com.basic

	method get_var_id (i : int) =
		i

	method read_expression_eagerly (cf : tclass_field) =
		false

	method make_lazy_type t f =
		TLazy (make_unforced_lazy t f "com-api")
end

let find_module ~(minimal_restore : bool) com cc path =
	(new hxb_reader_api_com ~minimal_restore com cc)#find_module path

type handler_context = {
	com : Common.context;
	jsonrpc : jsonrpc_handler;
	display : display_handler;
	send_result : Json.t -> unit;
	send_error : 'a . Json.t list -> 'a;
}

let handler =
	let open CompilationCache in
	let h = Hashtbl.create 0 in
	let l = [
		"initialize", (fun hctx ->
			supports_resolve := hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_bool_param "supportsResolve") false;
			ServerConfig.max_completion_items := hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_int_param "maxCompletionItems") 0;
			let exclude = hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_array_param "exclude") [] in
			DisplayToplevel.exclude := List.map (fun e -> match e with JString s -> s | _ -> die "" __LOC__) exclude;
			let methods = Hashtbl.fold (fun k _ acc -> (jstring k) :: acc) h [] in
			hctx.send_result (JObject [
				"methods",jarray methods;
				"haxeVersion",jobject [
					"major",jint version_major;
					"minor",jint version_minor;
					"patch",jint version_revision;
					"pre",(match version_pre with None -> jnull | Some pre -> jstring pre);
					"build",(match Version.version_extra with None -> jnull | Some(_,build) -> jstring build);
				];
				"protocolVersion",jobject [
					"major",jint 0;
					"minor",jint 5;
					"patch",jint 0;
				]
			])
		);
		"display/completionItem/resolve", (fun hctx ->
			let i = hctx.jsonrpc#get_int_param "index" in
			begin try
				let item = (!DisplayException.last_completion_result).(i) in
				let ctx = Genjson.create_context GMFull in
				hctx.send_result (jobject ["item",CompletionItem.to_json ctx None item])
			with Invalid_argument _ ->
				hctx.send_error [jstring (Printf.sprintf "Invalid index: %i" i)]
			end
		);
		"display/completion", (fun hctx ->
			hctx.display#set_display_file (hctx.jsonrpc#get_bool_param "wasAutoTriggered") true;
			hctx.display#enable_display DMDefault;
		);
		"display/definition", (fun hctx ->
			hctx.display#set_display_file false true;
			hctx.display#enable_display DMDefinition;
		);
		"display/diagnostics", (fun hctx ->
			hctx.display#set_display_file false false;
			hctx.display#enable_display ~skip_define:true DMNone;
			hctx.com.display <- { hctx.com.display with dms_display_file_policy = DFPAlso; dms_per_file = true; dms_populate_cache = true };
			hctx.com.report_mode <- RMDiagnostics (List.map (fun (f,_) -> f) hctx.com.file_contents);
		);
		"display/implementation", (fun hctx ->
			hctx.display#set_display_file false true;
			hctx.display#enable_display (DMImplementation);
		);
		"display/typeDefinition", (fun hctx ->
			hctx.display#set_display_file false true;
			hctx.display#enable_display DMTypeDefinition;
		);
		"display/references", (fun hctx ->
			hctx.display#set_display_file false true;
			match hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_string_param "kind") "normal" with
			| "withBaseAndDescendants" ->
				hctx.display#enable_display (DMUsage (false,true,true));
			| "withDescendants" ->
				hctx.display#enable_display (DMUsage (false,true,false));
			| _ ->
				hctx.display#enable_display (DMUsage (false,false,false));
		);
		"display/hover", (fun hctx ->
			hctx.display#set_display_file false true;
			hctx.display#enable_display DMHover;
		);
		"display/package", (fun hctx ->
			hctx.display#set_display_file false false;
			hctx.display#enable_display DMPackage;
		);
		"display/signatureHelp", (fun hctx ->
			hctx.display#set_display_file (hctx.jsonrpc#get_bool_param "wasAutoTriggered") true;
			hctx.display#enable_display DMSignature
		);
		"display/metadata", (fun hctx ->
			let include_compiler_meta = hctx.jsonrpc#get_bool_param "compiler" in
			let include_user_meta = hctx.jsonrpc#get_bool_param "user" in

			hctx.com.callbacks#add_after_init_macros (fun () ->
				let all = Meta.get_meta_list hctx.com.user_metas in
				let all = List.filter (fun (_, (data:Meta.meta_infos)) ->
					match data.m_origin with
					| Compiler when include_compiler_meta -> true
					| UserDefined _ when include_user_meta -> true
					| _ -> false
				) all in

				hctx.send_result (jarray (List.map (fun (t, (data:Meta.meta_infos)) ->
					let fields = [
						"name", jstring t;
						"doc", jstring data.m_doc;
						"parameters", jarray (List.map jstring data.m_params);
						"platforms", jarray (List.map (fun p -> jstring (platform_name p)) data.m_platforms);
						"targets", jarray (List.map (fun u -> jstring (Meta.print_meta_usage u)) data.m_used_on);
						"internal", jbool data.m_internal;
						"origin", jstring (match data.m_origin with
							| Compiler -> "haxe compiler"
							| UserDefined None -> "user-defined"
							| UserDefined (Some o) -> o
						);
						"links", jarray (List.map jstring data.m_links)
					] in

					(jobject fields)
				) all))
			)
		);
		"display/defines", (fun hctx ->
			let include_compiler_defines = hctx.jsonrpc#get_bool_param "compiler" in
			let include_user_defines = hctx.jsonrpc#get_bool_param "user" in

			hctx.com.callbacks#add_after_init_macros (fun () ->
				let all = Define.get_define_list hctx.com.user_defines in
				let all = List.filter (fun (_, (data:Define.define_infos)) ->
					match data.d_origin with
					| Compiler when include_compiler_defines -> true
					| UserDefined _ when include_user_defines -> true
					| _ -> false
				) all in

				hctx.send_result (jarray (List.map (fun (t, (data:Define.define_infos)) ->
					let fields = [
						"name", jstring t;
						"doc", jstring data.d_doc;
						"parameters", jarray (List.map jstring data.d_params);
						"platforms", jarray (List.map (fun p -> jstring (platform_name p)) data.d_platforms);
						"origin", jstring (match data.d_origin with
							| Compiler -> "haxe compiler"
							| UserDefined None -> "user-defined"
							| UserDefined (Some o) -> o
						);
						"deprecated", jopt jstring data.d_deprecated;
						"links", jarray (List.map jstring data.d_links)
					] in

					(jobject fields)
				) all))
			)
		);
		"server/resetCache", (fun hctx ->
			hctx.com.cs#clear;
			supports_resolve := false;
			DisplayException.reset();
			ServerConfig.reset();
			hctx.send_result (jobject [
				"success", jbool true
			]);
		);
		"server/readClassPaths", (fun hctx ->
			hctx.com.callbacks#add_after_init_macros (fun () ->
				let cc = hctx.display#get_cs#get_context (Define.get_signature hctx.com.defines) in
				cc#set_initialized true;
				DisplayToplevel.read_class_paths hctx.com ["init"];
				let files = hctx.display#get_cs#get_files in
				hctx.send_result (jobject [
					"files", jint (List.length files)
				]);
			)
		);
		"server/contexts", (fun hctx ->
			let l = List.map (fun cc -> cc#get_json) hctx.display#get_cs#get_contexts in
			let l = List.filter (fun json -> json <> JNull) l in
			hctx.send_result (jarray l)
		);
		"server/modules", (fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let cc = hctx.display#get_cs#get_context sign in
			let open HxbData in
			let l = Hashtbl.fold (fun _ m acc ->
				if m.mc_extra.m_kind <> MFake then jstring (s_type_path m.mc_path) :: acc else acc
			) cc#get_hxb [] in
			hctx.send_result (jarray l)
		);
		"server/module", (fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let path = Path.parse_path (hctx.jsonrpc#get_string_param "path") in
			let cs = hctx.display#get_cs in
			let cc = cs#get_context sign in
			let m = try
				find_module ~minimal_restore:true hctx.com cc path
			with Not_found ->
				hctx.send_error [jstring "No such module"]
			in
			hctx.send_result (generate_module (cc#get_hxb) (find_module ~minimal_restore:true hctx.com cc) m)
		);
		"server/type", (fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let path = Path.parse_path (hctx.jsonrpc#get_string_param "modulePath") in
			let typeName = hctx.jsonrpc#get_string_param "typeName" in
			let cc = hctx.display#get_cs#get_context sign in
			let m = try
				find_module ~minimal_restore:true hctx.com cc path
			with Not_found ->
				hctx.send_error [jstring "No such module"]
			in
			let rec loop mtl = match mtl with
				| [] ->
					hctx.send_error [jstring "No such type"]
				| mt :: mtl ->
					begin match mt with
					| TClassDecl c -> c.cl_restore()
					| _ -> ()
					end;
					let infos = t_infos mt in
					if snd infos.mt_path = typeName then begin
						let ctx = Genjson.create_context GMMinimum in
						hctx.send_result (Genjson.generate_module_type ctx mt)
					end else
						loop mtl
			in
			loop m.m_types
		);
		"server/typeContexts", (fun hctx ->
			let path = Path.parse_path (hctx.jsonrpc#get_string_param "modulePath") in
			let typeName = hctx.jsonrpc#get_string_param "typeName" in
			let contexts = hctx.display#get_cs#get_contexts in

			hctx.send_result (jarray (List.fold_left (fun acc cc ->
				match cc#find_module_opt path with
				| None -> acc
				| Some(m) ->
					let rec loop mtl = match mtl with
						| [] ->
							acc
						| mt :: mtl ->
							begin match mt with
							| TClassDecl c -> c.cl_restore()
							| _ -> ()
							end;
							if snd (t_infos mt).mt_path = typeName then
								cc#get_json :: acc
							else
								loop mtl
					in
					loop m.m_types
			) [] contexts))
		);
		"server/moduleCreated", (fun hctx ->
			let file = hctx.jsonrpc#get_string_param "file" in
			let file = Path.get_full_path file in
			let key = hctx.com.file_keys#get file in
			let cs = hctx.display#get_cs in
			List.iter (fun cc ->
				Hashtbl.replace cc#get_removed_files key (ClassPaths.create_resolved_file file hctx.com.empty_class_path)
			) cs#get_contexts;
			hctx.send_result (jstring file);
		);
		"server/files", (fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let cc = hctx.display#get_cs#get_context sign in
			let files = Hashtbl.fold (fun file cfile acc -> (file,cfile) :: acc) cc#get_files [] in
			let files = List.sort (fun (file1,_) (file2,_) -> compare file1 file2) files in
			let files = List.map (fun (fkey,cfile) ->
				jobject [
					"file",jstring cfile.c_file_path.file;
					"time",jfloat cfile.c_time;
					"pack",jstring (String.concat "." cfile.c_package);
					"moduleName",jopt jstring cfile.c_module_name;
				]
			) files in
			hctx.send_result (jarray files)
		);
		"server/invalidate", (fun hctx ->
			let file = hctx.jsonrpc#get_string_param "file" in
			let fkey = hctx.com.file_keys#get file in
			let cs = hctx.display#get_cs in
			cs#taint_modules fkey ServerInvalidate;
			cs#remove_files fkey;
			hctx.send_result jnull
		);
		"server/configure", (fun hctx ->
			let l = ref (List.map (fun (name,value) ->
				let value = hctx.jsonrpc#get_bool "value" value in
				try
					ServerMessage.set_by_name name value;
					jstring (Printf.sprintf "Printing %s %s" name (if value then "enabled" else "disabled"))
				with Not_found ->
					hctx.send_error [jstring ("Invalid print parame name: " ^ name)]
			) (hctx.jsonrpc#get_opt_param (fun () -> (hctx.jsonrpc#get_object_param "print")) [])) in
			hctx.jsonrpc#get_opt_param (fun () ->
				let b = hctx.jsonrpc#get_bool_param "noModuleChecks" in
				ServerConfig.do_not_check_modules := b;
				l := jstring ("Module checks " ^ (if b then "disabled" else "enabled")) :: !l;
				()
			) ();
			hctx.jsonrpc#get_opt_param (fun () ->
				let b = hctx.jsonrpc#get_bool_param "legacyCompletion" in
				ServerConfig.legacy_completion := b;
				l := jstring ("Legacy completion " ^ (if b then "enabled" else "disabled")) :: !l;
				()
			) ();
			hctx.send_result (jarray !l)
		);
		"server/memory",(fun hctx ->
			let j = Memory.get_memory_json hctx.display#get_cs MCache in
			hctx.send_result j
		);
		"server/memory/context",(fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let j = Memory.get_memory_json hctx.display#get_cs (MContext sign) in
			hctx.send_result j
		);
		"server/memory/module",(fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let path = Path.parse_path (hctx.jsonrpc#get_string_param "path") in
			let j = Memory.get_memory_json hctx.display#get_cs (MModule(sign,path)) in
			hctx.send_result j
		);
		(* TODO: wait till gama complains about the naming, then change it to something else *)
		"typer/compiledTypes", (fun hctx ->
			hctx.com.callbacks#add_after_filters (fun () ->
				let ctx = create_context GMFull in
				let l = List.map (generate_module_type ctx) hctx.com.types in
				hctx.send_result (jarray l)
			);
		);
	] in
	List.iter (fun (s,f) -> Hashtbl.add h s f) l;
	h

let parse_input com input report_times =
	let input =
		JsonRpc.handle_jsonrpc_error (fun () -> JsonRpc.parse_request input) send_json
	in
	let jsonrpc = new jsonrpc_handler input in

	let send_result json =
		flush stdout;
		flush stderr;
		let fl = [
			"result",json;
			"timestamp",jfloat (Unix.gettimeofday ());
		] in
		let fl = if !report_times then begin
			close_times();
			let _,_,root = Timer.build_times_tree () in
			begin match json_of_times root with
			| None -> fl
			| Some jo -> ("timers",jo) :: fl
			end
		end else fl in
		let fl = if DynArray.length com.pass_debug_messages > 0 then
			("passMessages",jarray (List.map jstring (DynArray.to_list com.pass_debug_messages))) :: fl
		else
			fl
		in
		let jo = jobject fl in
		send_json (JsonRpc.result jsonrpc#get_id  jo)
	in

	let send_error jl =
		send_json (JsonRpc.error jsonrpc#get_id 0 ~data:(Some (JArray jl)) "Compiler error")
	in

	com.json_out <- Some({
		send_result = send_result;
		send_error = send_error;
		jsonrpc = jsonrpc
	});

	let cs = com.cs in

	let display = new display_handler jsonrpc com cs in

	let hctx = {
		com = com;
		jsonrpc = jsonrpc;
		display = display;
		send_result = send_result;
		send_error = send_error;
	} in

	JsonRpc.handle_jsonrpc_error (fun () ->
		let method_name = jsonrpc#get_method_name in
		let f = try
			Hashtbl.find handler method_name
		with Not_found ->
			raise_method_not_found jsonrpc#get_id method_name
		in
		f hctx
	) send_json
