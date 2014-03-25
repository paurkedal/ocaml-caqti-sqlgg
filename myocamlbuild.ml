(* OASIS_START *)
(* OASIS_STOP *)

module My_options = struct
  let caqtigg = ref "src/caqtigg_main.native"
end

let caqtigg sql ml env build =
  let sql = env sql and ml = env ml in
  Cmd (S [P !My_options.caqtigg; P sql; A"-o"; Px ml])

let () = dispatch begin function

  | Before_options ->
    Options.use_ocamlfind := true

  | After_rules as e ->
    rule "sql -> ml" ~tags:["caqtigg"; "ocaml"]
         ~prods:["%.mli"; "%.ml"] ~deps:["%.sql"; !My_options.caqtigg]
	 (caqtigg "%.sql" "%.ml");

    flag ["doc"; "ocaml"; "extension:html"] &
      S[A"-charset"; A"utf8"; A"-t"; A"Caqti support for sqlgg"];
    dispatch_default e

  | e ->
    dispatch_default e

end
