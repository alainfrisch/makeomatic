open Makeomatic

module M = Make(struct

let extra_args = 
  [
    "*", "-I misc -I parser -I types -I typing -I runtime -I compile -I driver -I schema -package ulex,pcre,netstring,num -syntax camlp4o";
    "misc/q_symbol.ml", "-pp 'camlp4o pa_extend.cmo q_MLast.cmo'";
    "driver/run.ml", ppopt "-I misc q_symbol.cmo \
                            -symbol cduce_version=\\\"ABC\\\" \
                            -symbol build_date=\\\"X\\\" -symbol ocaml_compiler=\\\"N\\\"";

    "cduce.byte", "-I +camlp4 gramlib.cma";
    "cduce.opt", "-I +camlp4 gramlib.cmxa";
  ]

let packs =
  [ 
    "x", ["parser";"lexer";"calc"];
    "y", ["sub/a";"sub/b"];
    "z", ["x";"y"];
  ]

let execs =
  [ "cduce", ["driver/start"] ]


let extra_args = 
  let extra = List.map (fun (re,f) -> compile_glob re, f) extra_args in
  fun fn ->
    let args =
      List.fold_left (fun accu (re,f) -> if re fn then f::accu else accu) 
	[] extra in
    String.concat " " args




let ocaml cmd ~native args = match native,cmd with
  | false, `Compile src ->
      let args = extra_args src ^ " " ^ args in
      Printf.sprintf "ocamlfind ocamlc %s -c %s" args src
  | false, `Pack (obj,comps) ->
      let args = extra_args obj ^ " " ^ args in
      Printf.sprintf "ocamlfind ocamlc %s -pack -o %s %s" args obj comps
  | false, `Link (obj,comps) ->
      let args = extra_args obj ^ " " ^ args in
      Printf.sprintf "ocamlfind ocamlc %s -linkpkg -o %s %s" args obj comps
  | true, `Compile src ->
      let args = extra_args src ^ " " ^ args in
      Printf.sprintf "ocamlfind ocamlopt %s -c %s" args src
  | true, `Pack (obj,comps) ->
      let args = extra_args obj ^ " " ^ args in
      Printf.sprintf "ocamlfind ocamlopt %s -pack -o %s %s" args obj comps
  | true, `Link (obj,comps) ->
      let args = extra_args obj ^ " " ^ args in
      Printf.sprintf "ocamlfind ocamlopt %s -linkpkg -o %s %s" args obj comps


end)
