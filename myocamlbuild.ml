(* OASIS_START *)
(* OASIS_STOP *)

module My_options = struct
  let caqtigg = ref "src/caqtigg_main.native"
end

let caqtigg ~po sql ml env build =
  let sql = env sql and ml = env ml in
  let tags = Tags.union (tags_of_pathname sql) (tags_of_pathname ml) in
  Cmd (S [P !My_options.caqtigg; T (tags ++ "caqtigg"); A"-param-order"; A po;
	  P sql; A"-o"; Px ml])

let () = dispatch begin function

  | Before_options ->
    Options.use_ocamlfind := true

  | After_rules as e ->
    rule "sql -> _fx.ml" ~tags:["caqtigg-fx"; "ocaml"]
         ~prods:["%_fx.mli"; "%_fx.ml"] ~deps:["%.sql"; !My_options.caqtigg]
	 (caqtigg ~po:"fx" "%.sql" "%_fx.ml");
    rule "sql -> _Cfx.ml" ~tags:["caqtigg-Cfx"; "ocaml"]
         ~prods:["%_Cfx.mli"; "%_Cfx.ml"] ~deps:["%.sql"; !My_options.caqtigg]
	 (caqtigg ~po:"Cfx" "%.sql" "%_Cfx.ml");
    rule "sql -> _fxC.ml" ~tags:["caqtigg-fxC"; "ocaml"]
         ~prods:["%_fxC.mli"; "%_fxC.ml"] ~deps:["%.sql"; !My_options.caqtigg]
	 (caqtigg ~po:"fxC" "%.sql" "%_fxC.ml");

    flag ["doc"; "ocaml"; "extension:html"] &
      S[A"-charset"; A"utf8"; A"-t"; A"Caqti support for sqlgg"];
    dispatch_default e

  | e ->
    dispatch_default e

end
