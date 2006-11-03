open Makeomatic

module M = Make(struct

let extra_args = 
  [
    "*", "-I utils -I parsing -I typing -I bytecomp -I asmcomp -I driver \
         -I toplevel";
    "ocamlc.opt", "asmrun/meta.o asmrun/dynlink.o";
    "asmrun/*.c", "-DNATIVE_CODE";
    "*.c", "-I byterun";
  ]

let packs =
  [ ]

let execs =
  [ "ocamlc", ["driver/main"] ]


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
      Printf.sprintf "ocamlc %s -c %s" args src
  | false, `Pack (obj,comps) ->
      let args = extra_args obj ^ " " ^ args in
      Printf.sprintf "ocamlc %s -pack -o %s %s" args obj comps
  | false, `Link (obj,comps) ->
      let args = extra_args obj ^ " " ^ args in
      Printf.sprintf "ocamlc %s -o %s %s" args obj comps
  | true, `Compile src ->
      let args = extra_args src ^ " " ^ args in
      Printf.sprintf "ocamlopt %s -c %s" args src
  | true, `Pack (obj,comps) ->
      let args = extra_args obj ^ " " ^ args in
      Printf.sprintf "ocamlopt %s -pack -o %s %s" args obj comps
  | true, `Link (obj,comps) ->
      let args = extra_args obj ^ " " ^ args in
      Printf.sprintf "ocamlopt %s -o %s %s" args obj comps

let gcc src =
  let args = extra_args src in
  Printf.sprintf "gcc -c -o %s %s %s" (Filename.chop_extension src ^ ".o")
    args src

let rules = 
  [ function 
      | "bytecomp/opcodes.ml"
      | "utils/config.ml" 
      | "bytecomp/runtimedef.ml" 
      | "byterun/primitives"
	  as f -> Some (Printf.sprintf "rm -f %s && make %s" f f)
      | "asmrun/meta.c" | "asmrun/dynlink.c" as f
	  -> Some (Printf.sprintf "rm -f %s && cd asmrun && make %s"
		     f (Filename.basename f))
      | _ -> None ]

end)
