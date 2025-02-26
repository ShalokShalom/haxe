(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

(* Typing of functions and their arguments. *)

open Globals
open Ast
open Type
open Typecore
open Common
open Error
open FunctionArguments

let save_field_state ctx =
	let locals = ctx.f.locals in
	(fun () ->
		ctx.f.locals <- locals;
	)

let type_function_params ctx fd host fname =
	Typeload.type_type_params ctx host ([],fname) fd.f_params

let type_function ctx (args : function_arguments) ret e do_display p =
	ctx.e.ret <- ret;
	ctx.e.opened <- [];
	enter_field_typing_pass ctx.g ("type_function",fst ctx.c.curclass.cl_path @ [snd ctx.c.curclass.cl_path;ctx.f.curfield.cf_name]);
	args#bring_into_context ctx;
	let e = match e with
		| None ->
			if ignore_error ctx.com then
				(* when we don't care because we're in display mode, just act like
				   the function has an empty block body. this is fine even if function
				   defines a return type, because returns aren't checked in this mode
				*)
				EBlock [],p
			else
				if ctx.e.curfun = FunMember && has_class_flag ctx.c.curclass CAbstract then
					raise_typing_error "Function body or abstract modifier required" p
				else
					raise_typing_error "Function body required" p
		| Some e -> e
	in
	let is_position_debug = Meta.has (Meta.Custom ":debug.position") ctx.f.curfield.cf_meta in
	let e = if not do_display then begin
		if is_position_debug then print_endline ("syntax:\n" ^ (Expr.dump_with_pos e));
		type_expr ctx e NoValue
	end else begin
		let is_display_debug = Meta.has (Meta.Custom ":debug.display") ctx.f.curfield.cf_meta in
		if is_display_debug then print_endline ("before processing:\n" ^ (Expr.dump_with_pos e));
		let e = if !Parser.had_resume then e else Display.preprocess_expr ctx.com e in
		if is_display_debug then print_endline ("after processing:\n" ^ (Expr.dump_with_pos e));
		type_expr ctx e NoValue
	end in
	let e = match e.eexpr with
		| TMeta((Meta.MergeBlock,_,_), ({eexpr = TBlock el} as e1)) -> e1
		| _ -> e
	in
	let has_return e =
		let rec loop e =
			match e.eexpr with
			| TReturn (Some _) -> raise Exit
			| TFunction _ -> ()
			| _ -> Type.iter loop e
		in
		try loop e; false with Exit -> true
	in
	begin match follow ret with
		| TAbstract({a_path=[],"Void"},_) -> ()
		(* We have to check for the presence of return expressions here because
		   in the case of Dynamic ctx.ret is still a monomorph. If we indeed
		   don't have a return expression we can link the monomorph to Void. We
		   can _not_ use type_iseq to avoid the Void check above because that
		   would turn Dynamic returns to Void returns. *)
		| TMono m when not (has_return e) -> unify ctx ctx.t.tvoid ret p
		| _ -> (try TypeloadCheck.return_flow ctx e with Exit -> ())
	end;
	let rec loop e =
		match e.eexpr with
		| TCall ({ eexpr = TConst TSuper },_) -> raise Exit
		| TFunction _ -> ()
		| _ -> Type.iter loop e
	in
	let has_super_constr() =
		match ctx.c.curclass.cl_super with
		| None ->
			None
		| Some (csup,tl) ->
			try
				let cf = get_constructor csup in
				Some (Meta.has Meta.CompilerGenerated cf.cf_meta,TInst(csup,tl))
			with Not_found ->
				None
	in
	let e = if ctx.e.curfun <> FunConstructor then
		e
	else begin
		delay ctx.g PForce (fun () -> TypeloadCheck.check_final_vars ctx e);
		match has_super_constr() with
		| Some (was_forced,t_super) ->
			(try
				loop e;
				if was_forced then
					let e_super = mk (TConst TSuper) t_super e.epos in
					let e_super_call = mk (TCall(e_super,[])) ctx.t.tvoid e.epos in
					concat e_super_call e
				else begin
					display_error ctx.com "Missing super constructor call" p;
					e
				end
			with
				Exit -> e);
		| None ->
			e
	end in
	let e = match ctx.e.curfun, ctx.f.vthis with
		| (FunMember|FunConstructor), Some v ->
			let ev = mk (TVar (v,Some (mk (TConst TThis) ctx.c.tthis p))) ctx.t.tvoid p in
			(match e.eexpr with
			| TBlock l ->
				if ctx.com.config.pf_this_before_super then
					{ e with eexpr = TBlock (ev :: l) }
				else begin
					let rec has_v e = match e.eexpr with
						| TLocal v' when v' == v -> true
						| _ -> check_expr has_v e
					in
					let rec loop el = match el with
						| e :: el ->
							if has_v e then
								ev :: e :: el
							else
								e :: loop el
						| [] ->
							(* should not happen... *)
							[]
					in
					{ e with eexpr = TBlock (loop l) }
				end
			| _ -> mk (TBlock [ev;e]) e.etype p)
		| _ -> e
	in
	List.iter (fun r -> r := Closed) ctx.e.opened;
	let mono_debug = Meta.has (Meta.Custom ":debug.mono") ctx.f.curfield.cf_meta in
	if mono_debug then begin
		let pctx = print_context () in
		let print_mono i m =
			Printf.sprintf "%4i: %s" i (MonomorphPrinting.s_mono s_type pctx true m)
		in
		print_endline "BEFORE:";
		let monos = List.mapi (fun i (m,p) ->
			let s = print_mono i m in
			let spos = if p.pmin = -1 then
				"unknown"
			else begin
				let l1,p1,_,_ = Lexer.get_pos_coords p in
				Printf.sprintf "%i:%i" l1 p1
			end in
			print_endline (Printf.sprintf "%s (%s)" s spos);
			safe_mono_close ctx m p;
			(i,m,p,s)
		) ctx.e.monomorphs in
		print_endline "CHANGED:";
		List.iter (fun (i,m,p,s) ->
			let s' = print_mono i m in
			if s <> s' then begin
				print_endline s'
			end
		) monos
	end else
		List.iter (fun (m,p) -> safe_mono_close ctx m p) ctx.e.monomorphs;
	if is_position_debug then print_endline ("typing:\n" ^ (Texpr.dump_with_pos "" e));
	e

let type_function ctx args ret e do_display p =
	let save = save_field_state ctx in
	Std.finally save (type_function ctx args ret e do_display) p

let add_constructor ctx_c c force_constructor p =
	if c.cl_constructor <> None then () else
	let constructor = try Some (Type.get_constructor_class c (extract_param_types c.cl_params)) with Not_found -> None in
	match constructor with
	| Some(cfsup,csup,cparams) when not (has_class_flag c CExtern) ->
		let cf = mk_field "new" cfsup.cf_type p null_pos in
		cf.cf_kind <- cfsup.cf_kind;
		cf.cf_params <- cfsup.cf_params;
		cf.cf_meta <- List.filter (fun (m,_,_) -> m = Meta.CompilerGenerated) cfsup.cf_meta;
		let t = spawn_monomorph ctx_c p in
		let r = make_lazy ctx_c.g t (fun () ->
			let ctx = TyperManager.clone_for_field ctx_c cf cf.cf_params in
			ignore (follow cfsup.cf_type); (* make sure it's typed *)
			List.iter (fun cf -> ignore (follow cf.cf_type)) cf.cf_overloads;
			let map_arg (v,def) =
				(*
					let's optimize a bit the output by not always copying the default value
					into the inherited constructor when it's not necessary for the platform
				*)
				let null () = Some (Texpr.Builder.make_null v.v_type v.v_pos) in
				match ctx.com.platform, def with
				| _, Some _ when not ctx.com.config.pf_static -> v, null()
				| Flash, Some ({eexpr = TConst (TString _)}) when not (has_class_flag csup CExtern) -> v, null()
				| Cpp, Some ({eexpr = TConst (TString _)}) -> v, def
				| Cpp, Some _ -> { v with v_type = ctx.t.tnull v.v_type }, null()
				| _ -> v, def
			in
			let args = (match cfsup.cf_expr with
				| Some { eexpr = TFunction f } ->
					List.map map_arg f.tf_args
				| _ ->
					let values = get_value_meta cfsup.cf_meta in
					match follow cfsup.cf_type with
					| TFun (args,_) ->
						List.map (fun (n,o,t) ->
							let def = try
								type_function_arg_value ctx t (Some (PMap.find n values)) false
							with Not_found ->
								if o then Some (Texpr.Builder.make_null t null_pos) else None
							in
							map_arg (alloc_var (VUser TVOArgument) n (if o then ctx.t.tnull t else t) p,def) (* TODO: var pos *)
						) args
					| _ -> die "" __LOC__
			) in
			let p = c.cl_pos in
			let vars = List.map (fun (v,def) -> alloc_var (VUser TVOArgument) v.v_name (apply_params csup.cl_params cparams v.v_type) v.v_pos, def) args in
			let super_call = mk (TCall (mk (TConst TSuper) (TInst (csup,cparams)) p,List.map (fun (v,_) -> mk (TLocal v) v.v_type p) vars)) ctx.t.tvoid p in
			let constr = mk (TFunction {
				tf_args = vars;
				tf_type = ctx.t.tvoid;
				tf_expr = super_call;
			}) (TFun (List.map (fun (v,c) -> v.v_name, c <> None, v.v_type) vars,ctx.t.tvoid)) p in
			cf.cf_expr <- Some constr;
			cf.cf_type <- t;
			unify ctx t constr.etype p;
			t
		) "add_constructor" in
		cf.cf_type <- TLazy r;
		c.cl_constructor <- Some cf;
	| _ when force_constructor ->
		let constr = mk (TFunction {
			tf_args = [];
			tf_type = ctx_c.t.tvoid;
			tf_expr = mk (TBlock []) ctx_c.t.tvoid p;
		}) (tfun [] ctx_c.t.tvoid) p in
		let cf = mk_field "new" constr.etype p null_pos in
		cf.cf_expr <- Some constr;
		cf.cf_type <- constr.etype;
		cf.cf_meta <- [Meta.CompilerGenerated,[],null_pos];
		cf.cf_kind <- Method MethNormal;
		c.cl_constructor <- Some cf;
	| _ ->
		(* nothing to do *)
		()
;;
Typeload.type_function_params_ref := type_function_params
