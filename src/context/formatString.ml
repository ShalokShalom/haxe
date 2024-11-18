open Extlib_leftovers
open Globals
open Ast

let format_string defines s p process_expr =
	let len = String.length s in
	let get_next i =
		if i >= len then raise End_of_file else
		(UTF8.look s i, UTF8.next s i)
	in

	let read_char = ref 0 in
	let char_len = ref 0 in

	let get_next_char i =
		let (chr, next) = try get_next i
			with Invalid_argument _ ->
				raise End_of_file
		in

		try
			let c = UCharExt.char_of chr in
			incr read_char;
			c, (fun buf ->
				incr char_len;
				UTF8.Buf.add_char buf chr
			), next
		with UCharExt.Out_of_range ->
			let get i =
				let ch = String.unsafe_get s i in
				(ch, int_of_char ch)
			in
			let (ch, c) = get !read_char in

			let buf = Buffer.create 0 in
			Common.utf16_add buf c;
			let len = Buffer.length buf in

			read_char := !read_char + len;

			ch, (fun buf ->
				(* UTF16 handling *)
				if c >= 0x80 && c < 0x800 then begin
					let b = Buffer.create 0 in
					let add c = Buffer.add_char b (char_of_int (c land 0xFF)) in
					let c' = c lor (snd (get (i + 1)) lsl 8) in
					add c';
					add (c' lsr 8);

					let s' = Buffer.contents b in

					(* ok but why? *)
					if c' lsr 8 < 0x80 then char_len := !char_len + 2
					else if c' < 0xDFFF then incr char_len;

					UTF8.Buf.add_string buf s'
				end else
					die "" __LOC__;
			), i+len
	in

	let buf = UTF8.Buf.create len in
	let e = ref None in
	let pmin = ref p.pmin in
	let min = ref (p.pmin + 1) in

	let add_expr (enext,p) =
		min := !min + !char_len;
		char_len := 0;
		let enext = process_expr enext p in
		match !e with
		| None -> e := Some enext
		| Some prev ->
			e := Some (EBinop (OpAdd,prev,enext),punion (pos prev) p)
	in

	let add enext =
		let p = { p with pmin = !min; pmax = !min + !char_len } in
		add_expr (enext,p)
	in

	let add_sub () =
		let s = UTF8.Buf.contents buf in
		UTF8.Buf.clear buf;
		if !char_len > 0 || !e = None then add (EConst (String (s,SDoubleQuotes)))
	in

	let rec parse pos' =
		try begin
			let (c, store', pos) = get_next_char pos' in

			if c = '\'' then begin
				incr pmin;
				incr min;
			end;

			if c <> '$' || pos >= len then begin
				store' buf;
				parse pos
			end else
				let (c, store, pos) = get_next_char pos in
				match c with
				| '$' ->
					(* double $ *)
					store buf;
					add_sub ();
					parse pos
				| '{' ->
					add_sub ();
					parse_group pos' pos '{' '}' "brace"
				| 'a'..'z' | 'A'..'Z' | '_' ->
					add_sub ();
					incr min;
					let buf = UTF8.Buf.create len in
					store buf;
					let rec loop i =
						if i = len then i else
						let (c,store,next) = get_next_char i in

						match c with
						| 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' ->
							store buf;
							loop next
						| _ -> i
					in
					let iend = loop pos in
					let id = UTF8.Buf.contents buf in
					add (EConst (Ident id));
					parse iend
				| _ ->
					(* keep as-is *)
					store' buf;
					store buf;
					parse pos
		end with End_of_file -> add_sub ()

	and parse_group prev pos gopen gclose gname =
		let buf = UTF8.Buf.create len in
		let rec loop groups i =
			if i = len then
				match groups with
				| [] -> die "" __LOC__
				| g :: _ -> Error.raise_typing_error ("Unclosed " ^ gname) { p with pmin = !pmin + g + 1; pmax = !pmin + g + 2 }
			else
				let (c, store, pos) = get_next_char i in
				if c = gopen then begin
					store buf;
					loop (i :: groups) pos
				end else if c = gclose then begin
					let groups = List.tl groups in
					if groups = [] then pos else begin
						store buf;
						loop groups pos
					end
				end else begin
					store buf;
					loop groups pos
				end
		in
		let send = loop [prev] pos in
		let scode = UTF8.Buf.contents buf in
		min := !min + 2;
		begin
			let e =
				let ep = { p with pmin = !pmin + pos + 2; pmax = !pmin + send } in
				let error msg pos =
					if Lexer.string_is_whitespace scode then Error.raise_typing_error "Expression cannot be empty" ep
					else Error.raise_typing_error msg pos
				in
				match ParserEntry.parse_expr_string defines scode ep error true with
					| ParseSuccess(data,_,_) -> data
					| ParseError(_,(msg,p),_) -> error (Parser.error_msg msg) p
			in
			add_expr e
		end;
		min := !min + 1;
		parse send
	in

	parse 0;
	match !e with
	| None -> die "" __LOC__
	| Some e -> e
