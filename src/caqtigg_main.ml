(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Printf

let param_order = ref "Cfx"
let use_functor = ref false

let sqlgg = ref "sqlgg"
let monad_module = ref "Lwt"
let connect_module = ref "Caqti_lwt"
let add_docstrings = ref true

let param_re = "\\?|@[a-zA-Z][a-zA-Z0-9_]*"
let is_param = function "?" -> true | s -> s.[0] = '@'

let iter_option f = function Some x -> f x | None -> ()

type cardinals = [`Zero | `Zero_one | `One | `Nat]

let type_of_sqltype = function
  | "Bool" -> "bool"
  | "Int" -> "int"
  | "Float" -> "float"
  | "Text" -> "string"
  | "Datetime" -> "utc"
  | s -> ksprintf invalid_arg "Type %s reported by sqlgg is not supported." s
let conv_of_sqltype = function
  | "Text" -> "text"
  | s -> type_of_sqltype s

(* SQL Fixup *)

let pgsql_rex = Pcre.regexp (param_re ^ "|\\bbinary\\b")

let pgsql_of_sql sql =
  let i_r = ref 0 in
  let subst = function
    | "binary" -> "bytea"
    | s when is_param s -> i_r := succ !i_r; sprintf "$%d" !i_r
    | _ -> assert false in
  Pcre.substitute ~rex:pgsql_rex ~subst sql

let sqlite_of_sql sql = sql

let dialects = [
  "`Pgsql", pgsql_of_sql;
  "`Sqlite", sqlite_of_sql;
]

let comment_rex = Pcre.regexp "(?<=\\()(?=\\*)|(?<=\\*)(?=\\))"
let comment_itempl = Pcre.subst " "
let comment_of_sql sql =
  Pcre.replace ~rex:comment_rex ~itempl:comment_itempl sql

(* Code Generation *)

let emit_prologue oc_mli oc_ml =
  iter_option
    (fun oc ->
      fprintf oc "open %s\n\n" !connect_module;
      if !use_functor then
	output_string oc "module Make (C : CONNECTION) : sig\n\n")
    oc_mli;
  iter_option
    (fun oc ->
      List.iter (fprintf oc "open %s\n")
		[!monad_module; "Caqti_query"; !connect_module];
      output_string oc "\n\
	let _query_info (module C : CONNECTION) = function\n\
	\  | Oneshot qs -> `Oneshot (qs C.backend_info)\n\
	\  | Prepared qs -> `Prepared (qs.pq_name, \
				       qs.pq_encode C.backend_info)\n\
	let _required (module C : CONNECTION) q = function\n\
	\  | Some r -> return r\n\
	\  | None ->\n\
	\    fail (Caqti.Miscommunication (C.uri, _query_info (module C) q, \
				\"Received no tuples, expected one.\"))\n\n";
      if !use_functor then
	output_string oc "module Make (C : CONNECTION) = struct\n\n")
    oc_ml

let emit_epilogue oc_mli oc_ml =
  if !use_functor then begin
    iter_option (fun oc -> output_string oc "end\n") oc_mli;
    iter_option (fun oc -> output_string oc "end\n") oc_ml
  end

let emit_intf_params emit_callback_param oc ivs =
  let emit_param (idr, typ) =
    output_string oc (type_of_sqltype typ);
    output_string oc " -> " in
  if ivs = [] && emit_callback_param = None && !use_functor then
    output_string oc "unit -> "
  else
    String.iter
      (function
	| 'C' -> output_string oc "(module CONNECTION) -> "
	| 'f' -> (match emit_callback_param with None -> () | Some f -> f ())
	| 'x' -> List.iter emit_param ivs
	| _ -> assert false)
      !param_order

let emit_intf_exec oc name sql ivs =
  fprintf oc "val %s : " name;
  emit_intf_params None oc ivs;
  output_string oc "unit io\n";
  if !add_docstrings then fprintf oc "(** [%s] *)\n\n" (comment_of_sql sql)

let emit_intf_single is_opt oc name sql ivs ovs =
  fprintf oc "val %s : " name;
  emit_intf_params None oc ivs;
  if ovs = [] then output_string oc "unit"
  else begin
    if List.tl ovs <> [] then output_char oc '(';
    List.iteri
      (fun i (idr, typ) ->
	if i > 0 then output_string oc " * ";
	output_string oc (type_of_sqltype typ))
      ovs;
    if List.tl ovs <> [] then output_char oc ')'
  end;
  output_string oc (if is_opt then " option io\n" else " io\n");
  if !add_docstrings then fprintf oc "(** [%s] *)\n\n" (comment_of_sql sql)

let emit_intf_multi op mint mext oc name sql ivs ovs =
  fprintf oc "val %s_%s : " op name;
  let emit_callback_params () =
    output_char oc '(';
    List.iter
      (fun (idr, typ) ->
	output_string oc (type_of_sqltype typ);
	output_string oc " -> ")
      ovs;
    output_string oc mint;
    output_string oc ") -> " in
  emit_intf_params (Some emit_callback_params) oc ivs;
  output_string oc mext;
  output_char oc '\n';
  if !add_docstrings then fprintf oc "(** [%s] *)\n\n" (comment_of_sql sql)

let emit_intf oc name sql cardinals ivs ovs =
  match cardinals with
  | `Zero -> emit_intf_exec oc name sql ivs
  | `Zero_one -> emit_intf_single true oc name sql ivs ovs
  | `One -> emit_intf_single false oc name sql ivs ovs
  | `Nat ->
    emit_intf_multi "fold" "'a -> 'a" "'a -> 'a io" oc name sql ivs ovs;
    emit_intf_multi "fold_s" "'a -> 'a io" "'a -> 'a io" oc name sql ivs ovs;
    emit_intf_multi "iter_s" "unit io" "unit io" oc name sql ivs ovs;
    emit_intf_multi "iter_p" "unit io" "unit io" oc name sql ivs ovs

let emit_impl_formals has_callback oc ivs =
  let emit_param (idr, typ) = output_char oc ' '; output_string oc idr in
  if ivs = [] && not has_callback && !use_functor then
    output_string oc " ()"
  else
    String.iter
      (function
	| 'C' -> output_string oc " (module C : CONNECTION)"
	| 'f' -> if has_callback then output_string oc " f"
	| 'x' -> List.iter emit_param ivs
	| _ -> assert false)
      !param_order

let emit_impl_params oc ivs =
  output_string oc "C.Param.([|";
  List.iteri
    (fun i (idr, typ) ->
      if i > 0 then output_string oc "; ";
      fprintf oc "%s %s" (conv_of_sqltype typ) idr)
    ivs;
  output_string oc "|])"

let emit_impl_exec oc name ivs =
  fprintf oc "let %s" name;
  emit_impl_formals false oc ivs;
  fprintf oc " =\n  C.exec _%s " name;
  emit_impl_params oc ivs;
  output_char oc '\n'

let emit_impl_decode oc ovs =
  output_string oc "  let g r = C.Tuple.(";
  List.iteri
    (fun i (idr, typ) ->
      if i > 0 then output_string oc ", ";
      fprintf oc "%s %d r" (conv_of_sqltype typ) i)
    ovs;
  output_string oc ") in\n"

let emit_impl_zero_one is_opt oc name ivs ovs =
  fprintf oc "let %s" name;
  emit_impl_formals false oc ivs;
  output_string oc " =\n";
  emit_impl_decode oc ovs;
  fprintf oc "  C.find _%s g " name;
  emit_impl_params oc ivs;
  output_char oc '\n'

let emit_impl_one is_opt oc name ivs ovs =
  fprintf oc "let %s" name;
  emit_impl_formals false oc ivs;
  output_string oc " =\n";
  emit_impl_decode oc ovs;
  fprintf oc "  C.find _%s g " name;
  emit_impl_params oc ivs;
  (* FIXME: The following shall raise Miscommunication. *)
  fprintf oc " >>= _required (module C) _%s" name

let emit_impl_multi op oc name ivs ovs =
  fprintf oc "let %s_%s" op name;
  emit_impl_formals true oc ivs;
  fprintf oc "=\n  let g r = C.Tuple.(f";
  List.iteri
    (fun i (idr, typ) -> fprintf oc " (%s %d r)" (conv_of_sqltype typ) i)
    ovs;
  output_string oc ") in\n";
  fprintf oc "  C.%s _%s g " op name;
  emit_impl_params oc ivs;
  output_char oc '\n'

let emit_impl oc name sql cardinals ivs ovs =
  fprintf oc "let _%s = prepare_fun @@ function\n" name;
  List.iter
    (fun (c, f) -> fprintf oc "  | %s -> \"%s\"\n" c (String.escaped (f sql)))
    dialects;
  fprintf oc "  | _ -> raise Missing_query_string\n";
  begin match cardinals with
  | `Zero -> emit_impl_exec oc name ivs
  | `Zero_one -> emit_impl_zero_one true oc name ivs ovs
  | `One -> emit_impl_one false oc name ivs ovs
  | `Nat ->
    emit_impl_multi "fold" oc name ivs ovs;
    emit_impl_multi "fold_s" oc name ivs ovs;
    emit_impl_multi "iter_s" oc name ivs ovs;
    emit_impl_multi "iter_p" oc name ivs ovs
  end;
  output_char oc '\n'

let emit_code mli_oc ml_oc name sql cardinals ivs ovs () =
  iter_option (fun oc -> emit_intf oc name sql cardinals ivs ovs) mli_oc;
  iter_option (fun oc -> emit_impl oc name sql cardinals ivs ovs) ml_oc

(* Scanning sqlgg output *)

let rec extract_values vs = function
  | [] -> vs
  | `El (((_, "value"), attrs), []) :: els ->
    let name = List.assoc ("", "name") attrs in
    let typ = List.assoc ("", "type") attrs in
    extract_values ((name, typ) :: vs) els
  | `Data _ :: els -> extract_values vs els
  | _ -> assert false

let rec extract_io ivs ovs = function
  | [] -> (ivs, ovs)
  | `El (((_, "in"), _), els) :: r -> extract_io (extract_values ivs els) ovs r
  | `El (((_, "out"), _), els) :: r -> extract_io ivs (extract_values ovs els) r
  | `Data _ :: r -> extract_io ivs ovs r
  | _ -> assert false

let scan_stmt f = function
  | `El (((_, "stmt"), attrs), els) ->
    let name = List.assoc ("", "name") attrs in
    let sql = List.assoc ("", "sql") attrs in
    let cardinals =
      match List.assoc ("", "cardinality") attrs with
      | "0" -> `Zero
      | "0,1" -> `Zero_one
      | "1" -> `One
      | "n" -> `Nat
      | _ -> assert false in
    let rev_inputs, rev_outputs = extract_io [] [] els in
    f name sql cardinals (List.rev rev_inputs) (List.rev rev_outputs)
  | _ -> assert false

let rec scan_sqlgg f xic acc =
  match Xmlm.input xic with
  | `Dtd _ -> scan_sqlgg f xic acc
  | `El_start ((_, "sqlgg"), _) ->
    let rec loop acc =
      match Xmlm.peek xic with
      | `El_start _ ->
	let el tag els : 'a Xmlm.frag as 'a = `El (tag, els) in
	let data s = `Data s in
	loop (scan_stmt f (Xmlm.input_tree ~el ~data xic) acc)
      | `El_end -> ignore (Xmlm.input xic); acc
      | `Data _ -> ignore (Xmlm.input xic); loop acc
      | _ -> assert false in
    loop acc
  | _ -> assert false

(* Main *)

let sh_escaped arg =
  let buf = Buffer.create (String.length arg + 2) in
  Buffer.add_char buf '\'';
  String.iter
    (function '\\' -> Buffer.add_string buf "\\\\"
	    | '"' ->  Buffer.add_string buf "\\\""
	    | c -> Buffer.add_char buf c)
    arg;
  Buffer.add_char buf '\'';
  Buffer.contents buf

let () =
  let rev_inputs_r = ref [] in
  let sqlgg_args_r = ref ["-gen"; "xml"] in
  let ml_out_r = ref None in
  let gen_r = ref `Auto in
  let set_param_order s =
    param_order := s;
    use_functor :=
      match s with
      | "fx" | "xf" -> true
      | "Cfx" | "fCx" | "fxC" | "Cxf" | "xCf" | "xfC" -> false
      | _ -> raise (Arg.Bad "Invalid parameter order.") in
  Arg.parse
    [ "-name",
	Arg.String (fun s -> sqlgg_args_r := "-name" :: sh_escaped s
						     :: !sqlgg_args_r),
	"<identifier> Passed to sqlgg.";
      "-gen",
	Arg.String (function "mli" -> gen_r := `mli
			   | "ml" -> gen_r := `ml
			   | _ -> raise (Arg.Bad "Unsupported output type.")),
	"mli|ml Type of output to generate.";
      "-param-order",
	Arg.String set_param_order,
	"Cfx|fCx|fxC|fx Specify argument order for the generated functions.  \
	    'C' represents the connection, \
	    'f' represents the callback where relevant, and \
	    'x' represents the query parameters.  \
	    If 'C' is omitted, the code functorised on the connection.";
      "-o", Arg.String (fun s -> ml_out_r := Some s),
	"PATH Write the output to PATH instead of standard output."; ]
    (fun s -> rev_inputs_r := s :: !rev_inputs_r)
    (Sys.argv.(0) ^ " <input.sql>+");
  let sqlgg_args = !sqlgg_args_r @ List.rev !rev_inputs_r in
  let sqlgg_command =
    String.concat " " (!sqlgg :: List.map sh_escaped sqlgg_args) in
  let ic = Unix.open_process_in sqlgg_command in
  let mli_oc, ml_oc =
    match !gen_r, !ml_out_r with
    | (`Auto | `ml), None -> None, Some stdout
    | `mli, None -> Some stdout, None
    | `mli, Some p -> Some (open_out p), None
    | `ml, Some p -> None, Some (open_out p)
    | `Auto, Some p ->
      Some (open_out (Filename.chop_suffix p ".ml" ^ ".mli")),
      Some (open_out p) in
  begin
    try
      let xic = Xmlm.make_input (`Channel ic) in
      emit_prologue mli_oc ml_oc;
      scan_sqlgg (emit_code mli_oc ml_oc) xic ();
      emit_epilogue mli_oc ml_oc
    with Xmlm.Error ((lnum, cnum), error) ->
      eprintf "%d:%d: %s\n" lnum cnum (Xmlm.error_message error);
      exit 69
  end;
  iter_option
    (fun _ -> iter_option close_out mli_oc; iter_option close_out ml_oc)
    !ml_out_r;
  match Unix.close_process_in ic with
  | Unix.WEXITED rc -> exit rc
  | Unix.WSIGNALED sn -> Unix.kill (Unix.getpid ()) sn
  | Unix.WSTOPPED _ -> assert false
