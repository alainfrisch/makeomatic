(* Command line *)

module CmdLine = struct
  open Arg
  let dump_cache = ref false
  let clear_cache = ref false
  let dump_genfiles = ref false
  let cache_file = ref "interactive.cache"
  let clean = ref false
  let targets = ref []
  let vars = ref []
  let tags = ref []
  let trace = ref false

  let specs = 
    Arg.align
      [ "--dump", Set dump_cache, 
	" Dump the content of the cache";

	"--clear", Set clear_cache, 
	" Clear the cache";

	"--cache", Set_string cache_file, 
	"<file> Choose a different cache file";

	"--genfiles", Set dump_genfiles,
	" Print a list of generated files";

	"--clean", Set clean,
	" Remove generated files";

	"--trace", Set trace,
	" Trace captured syscalls";
      ]

  let usage = "Usage:\n  makeomatic <options> [target | +tag | var=val ...]\n"

  let () =
    parse specs
	(fun s -> 
	   if s.[0] = '+' 
	   then tags := String.sub s 1 (String.length s - 1) :: !tags
	   else try
	     let eq = String.index s '=' in
	     vars := (String.sub s 0 eq, 
		      String.sub s (eq+1) (String.length s - eq - 1)) :: !vars;
	   with Not_found ->
	     targets := s :: !targets) usage

  let targets = List.rev !targets
end

(* Persistent cache *)

let cache_file = !CmdLine.cache_file

let file_cache, digest_cache as cache_state =
  if (not !CmdLine.clear_cache) && Sys.file_exists cache_file then
    let ic = open_in_bin cache_file in
    let v = input_value ic in
    close_in ic;
    v
  else Hashtbl.create 1, Hashtbl.create 1


let save_cache () =
  let oc = open_out_bin cache_file in
  output_value oc cache_state;
  close_out oc

(* Digest computation *)

let digest fn =
  let st = Unix.stat fn in
  let info = (st.Unix.st_mtime, st.Unix.st_ino) in
  try 
    let (md5,info') = Hashtbl.find digest_cache fn in
    if info <> info' then raise Not_found;
    md5
  with Not_found -> 
    let md5 = Digest.file fn in
    if !CmdLine.trace then Format.eprintf "MD5(%s)@." fn;
    Hashtbl.replace digest_cache fn (md5,info);
    md5

let force_digest = digest


let depth = ref 0

(* Printing information *)

let short_digest s = String.sub (Digest.to_hex s) 0 4

let rec print_dep ppf = function
  | `File (fn, `Absent) ->
      Format.fprintf ppf "-%s" fn
  | `File (fn, `Digest md5) ->
      Format.fprintf ppf "+%s[%s]" fn (short_digest md5)
  | `File (fn, `Present) ->
      Format.fprintf ppf "+%s" fn

let print_deps ppf = 
  List.iter (fun d -> Format.fprintf ppf "  %a@." print_dep d) 

let dump_cache ppf =
  Hashtbl.iter
    (fun file (cmds,md5) ->
       Format.fprintf ppf "%s[%s]:@." file (short_digest md5);
       List.iter 
	 (fun (cmdline,deps) ->
	    Format.fprintf ppf " %S:@.%a" cmdline print_deps deps)
	 cmds)
    file_cache;
  Hashtbl.iter
    (fun file (md5,(mtime,inode)) ->
       Format.fprintf ppf "%s[%s] at %f, inode=%i@." 
	 file (short_digest md5) mtime inode)
    digest_cache

let dump_genfiles ppf =
  Hashtbl.iter
    (fun file _ ->
       Format.fprintf ppf "%s " file)
    file_cache;
  Format.fprintf ppf "@."



(* Instrumented command evaluation *)

let test_prefix s pr =
  let l = String.length pr in
  if (String.length s >= l) && (String.sub s 0 l = pr) 
  then Some (String.sub s l (String.length s - l))
  else None

let extract_word s =
  let i = String.index s ' ' in
  String.sub s 0 i, String.sub s (succ i) (String.length s - i - 1)

let ign s =
  not (Filename.is_relative s) || s = "."

let extract_cmd s =
  match test_prefix s "##[MAKEOMATIC]## " with None -> None | Some cmd0 ->
    let cmd,arg = extract_word cmd0 in
    match cmd with
      | "__xstat64" ->
	  if ign arg then Some `Ignore
	  else Some (`Check arg)
      | "access" -> 
	  let _,arg = extract_word arg in
	  if ign arg then Some `Ignore
	  else Some (`Check arg)
      | "open64" | "open" ->
	  let flags,arg = extract_word arg in
	  if ign arg then Some `Ignore
	  else
	  let flags = int_of_string flags in
	  let wronly = flags land 1 <> 0 
	  and rdwr = flags land 2 <> 0
	  and creat = flags land 64 <> 0 in
	  if creat then Some (`Out arg)
	  else if not wronly && not rdwr then Some (`In arg)
	  else Some (`Other (arg,cmd0))
      | "fopen" | "fopen64" ->
	  let mode,arg = extract_word arg in
	  if ign arg then Some `Ignore
	  else (match mode with
	     | "w" | "w+" | "w+b" -> Some (`Out arg)
	     | "r" | "rb" -> Some (`In arg)
	     | _ -> Some (`Other (arg,cmd0))
	  )
      | cmd -> 
	  Format.eprintf "** Unknown command: %s@. Aborting." cmd; 
	  save_cache (); exit 2

let fifo_num = ref 0

let run_background cmdline = 
  match Unix.fork() with
    | 0 -> Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmdline |]
    | id -> id

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

let run_instrumented cmdline callback =
  incr fifo_num;
  let fifo_cmd_name = Printf.sprintf "CMD%i" !fifo_num in
  let fifo_ping_name = Printf.sprintf "PING%i" !fifo_num in

  Unix.mkfifo fifo_cmd_name 0o777;
  Unix.mkfifo fifo_ping_name 0o777;
  at_exit (fun () -> Sys.remove fifo_cmd_name; Sys.remove fifo_ping_name);

  Unix.putenv "MAKEOMATIC_FIFO_CMD" fifo_cmd_name;
  Unix.putenv "MAKEOMATIC_FIFO_PING" fifo_ping_name;

  incr depth;
  let pid = run_background cmdline in
  let cmd = open_in fifo_cmd_name in
  let ping = open_out fifo_ping_name in

  (try while true do 
     let s = input_line cmd in
     if !CmdLine.trace then print_endline s;
     match extract_cmd s with
       | None -> print_endline s
       | Some cmd -> 
	   callback cmd; 
	   output_string ping "\n"; flush ping
   done with End_of_file -> ());
  decr depth;
  match snd(waitpid_non_intr pid) with
    | Unix.WEXITED 0 -> true
    | _ -> false

(* Produce the most up-to-date version of file [fn]. *)

let build_rules = ref []

let already_built = Hashtbl.create 1
let already_run = Hashtbl.create 1

let absents = Hashtbl.create 1

let sort_deps =
  List.sort
    (fun (`File (_,x)) (`File (_,y)) -> match x,y with
       | `Digest _, (`Present | `Absent)
       | `Present, `Absent -> (-1)
       | _ -> 1)

let rec build fn =
  if fn.[0] = '/' then Sys.file_exists fn
  else let () = if !CmdLine.trace then Format.eprintf "BUILD(%s)@." fn in
  if Hashtbl.mem already_built fn then Sys.file_exists fn 
  else let () = Hashtbl.add already_built fn () in
  (match cmdline_for_file fn with
     | None -> ()
     | Some cmdline ->
	 if need_to_run fn cmdline then interactive fn cmdline
(*	 else Format.eprintf "%s <- (%s)@." fn cmdline*));
  if not (Sys.file_exists fn) then (Hashtbl.add absents fn (); false)
  else true
      
and cmdline_for_file fn =
  let rec aux = function
    | [] -> None
    | rule :: rest -> match rule fn with
	| Some _ as x -> x
	| None -> aux rest in
  aux !build_rules

and build_digest fn =
  if build fn then `File (fn,`Digest (digest fn))
  else `File (fn, `Absent)

and build_present fn =
  if build fn then `File (fn,`Present)
  else `File (fn, `Absent)

and check_dep = function
  | `File (fn, `Absent) -> not (build fn)
  | `File (fn, `Digest md5) -> build fn && digest fn = md5
  | `File (fn, `Present) -> build fn

and check_deps l = List.for_all check_dep l

and need_to_run fout cmdline =
  if Hashtbl.mem already_run cmdline then false
  else try
    let (cmds,md5) = Hashtbl.find file_cache fout in
    if (not (Sys.file_exists fout)) || (md5 <> digest fout) then
      (Hashtbl.remove file_cache fout; raise Not_found);
    let deps = List.assoc cmdline cmds in
    if not (check_deps deps) then
      (Hashtbl.replace file_cache fout (List.remove_assoc cmdline cmds,md5);
       raise Not_found);
    false
  with Not_found ->
    Hashtbl.add already_run cmdline ();
    true

and interactive fout cmdline =
  for i = 1 to !depth do Format.eprintf " " done;
  Format.eprintf "%s@." cmdline;

  let deps = Hashtbl.create 1 in
  let outputs = ref [] in
  let reg fn d = Hashtbl.replace deps fn d in
  let instrument = function
    | `Check fn ->
	if not (List.mem fn !outputs) && not (Hashtbl.mem deps fn)
	then reg fn (build_present fn)
    | `Out fn ->
	if Hashtbl.mem absents fn then (
	  Format.eprintf 
	    "\
** File %s now created, but it has been requested before and I
** had not been able to create it at that time.
** Some rule are missing!
** Still continuing.@."
	    fn;
	  Hashtbl.clear absents;
	  Hashtbl.clear already_built;
	);
	outputs := fn :: !outputs
    | `In fn ->
	if not (List.mem fn !outputs)
	then reg fn (build_digest fn);
    | `Other (fn,cmd) ->
	if not (List.mem fn !outputs)
	then Format.eprintf "** Ignoring %s@." cmd
    | `Ignore -> 
	() in
  let ok = run_instrumented cmdline instrument in
  if not ok then (
    Format.eprintf "** Aborting@."; 
    save_cache (); 
    exit 2
  );

  let deps = sort_deps (Hashtbl.fold (fun _ d accu -> d :: accu) deps []) in
  let outputs = List.filter Sys.file_exists !outputs in
  List.iter
    (fun fn ->
       let d = force_digest fn in
       let cmds =
	 try 
	   let (cmds,md5) = Hashtbl.find file_cache fn in 
	 if md5 = d then cmds else []
	 with Not_found -> []
       in
       Hashtbl.replace file_cache fn ((cmdline,deps)::cmds,d)
    )
    outputs

let main () =
  if !CmdLine.clean then (
    Hashtbl.iter
      (fun file _ -> if Sys.file_exists file then Sys.remove file)
      file_cache;
    Hashtbl.clear file_cache;
    Hashtbl.clear digest_cache;
  );
  if !CmdLine.dump_cache then (
    dump_cache Format.std_formatter;
    exit 0
  );
  if !CmdLine.dump_genfiles then (
    dump_genfiles Format.std_formatter;
    exit 0
  );
  Unix.putenv "LD_PRELOAD" (Sys.argv.(0) ^ "_wrapper.so");
  List.iter 
    (fun fn -> 
       if not (build fn) then 
	 Format.eprintf "** target %s cannot be built@." fn
    )
    CmdLine.targets;
  save_cache ()

(* Variables *)

module Tags = Set.Make(String)

type tags = Tags.t
let (+=) env v = Tags.add v env
let (-=) env v = Tags.remove v env
let (///) = Tags.union
let (!?) v env = Tags.mem v env
let (~~~) f env = not (f env)
let (&&&) f1 f2 env = f1 env && f2 env
let (|||) f1 f2 env = f1 env || f2 env

let tag_filter_rules : (tags -> tags) list ref = ref []
let files_rules : (tags -> string -> tags) list ref = ref []
let flags_rules : (string list * string list) list ref = ref []

let filter_tags tags = 
  List.fold_left (fun tags rule -> rule tags) tags !tag_filter_rules
let wrap_filter_tags tags kind =
  (filter_tags (tags += kind)) -= kind

let tags_cmdline = 
  wrap_filter_tags 
    (List.fold_left (+=) Tags.empty !CmdLine.tags) "tags_cmdline"


let tags_of_file_memo = Hashtbl.create 1

let tags_of_file (filename : string) =
  try Hashtbl.find tags_of_file_memo filename
  with Not_found ->
    let tags = 
      List.fold_left (fun tags rule -> rule tags filename) tags_cmdline 
	!files_rules in
    let tags = wrap_filter_tags tags "tags_file" in
    Hashtbl.add tags_of_file_memo filename tags;
    tags

let get_flags fn extra =
  let tags = filter_tags (List.fold_left (+=) (tags_of_file fn) extra) in
  let flags =  List.flatten (List.map 
			       (fun (cond,flags) -> 
				  if List.for_all (fun x -> Tags.mem x tags)
				    cond then flags else [])
			       !flags_rules) in
  if flags = [] then "" else (String.concat " " flags ^ " ")

(* Helpers to write rules *)

let depends file =
  try 
    let (cmds,_)  = Hashtbl.find file_cache file in
    let (_,deps) = List.hd cmds in
    List.fold_left
      (fun accu d -> match d with
	 | `File (f, `Digest _) -> f :: accu
	 | _ -> accu)
      [] deps
  with Not_found -> []

let test_ext fn ext =
  if Filename.check_suffix fn ext then Some (Filename.chop_suffix fn ext)
  else None

let ext fn ext1 ext2 =
  match test_ext fn ext1 with Some fn -> Some (fn ^ ext2) | None -> None

let make ext1 ext2 cmd fn =
  match ext fn ext1 ext2 with 
    | None -> None
    | Some f -> 
	if build f 
	then Some (cmd f fn) 
	else None

let from fout fins f fn =
  if fn = fout && List.for_all build fins then Some (f fins fout) else None

let concat args =
  String.concat " " (List.filter (fun s -> s <> "") args)

(* Rules *)

let ocaml_closure obj fns =
  let needed = ref [] in
  let seen = ref [] in
  let rec aux fn =
    if List.mem fn !needed then ()
    else if List.mem fn !seen then 
      (Printf.eprintf "** Circular dependency when linking %s. Aborting.\n" fn;
       save_cache ();
       exit 2
      )
    else (
      seen := fn :: !seen;
      if build fn then (
	List.iter
	  (fun f ->
	     match ext f ".cmi" obj with 
	       | None -> ()
	       | Some f -> if f <> fn then aux f)
	  (depends fn);
	needed := fn :: !needed
      )
    ) in
  List.iter aux fns;
  List.rev !needed

let ocamlc src target =
  Printf.sprintf "ocamlfind ocamlc -c %s%s"
    (get_flags src ["ocaml";"compile";"byte"])
    src

let ocamlc_pack target =
  Printf.sprintf "ocamlfind ocamlc -pack -c %s-o %s"
    (get_flags target ["ocaml";"byte"])
    target

let ocamlopt_pack target =
  Printf.sprintf "ocamlfind ocamlopt -pack -c %s-o %s"
    (get_flags target ["ocaml";"opt"])
    target

let ocamlopt src target =
  Printf.sprintf "ocamlfind ocamlopt -c %s%s"
    (get_flags src ["ocaml";"compile";"opt"])
    src

let ocamlc_link src target =
  let srcs = ocaml_closure ".cmo" [src] in
  Printf.sprintf "ocamlfind ocamlc -linkpkg %s -o %s %s"
    (get_flags target ["ocaml";"compile";"opt"])
    target (String.concat " " srcs)

let ocamlopt_link src target =
  let srcs = ocaml_closure ".cmx" [src] in
  Printf.sprintf "ocamlfind ocamlopt -linkpkg %s -o %s %s"
    (get_flags target ["ocaml";"compile";"opt"])
    target (String.concat " " srcs)

let gcc src target =
  Printf.sprintf "gcc -c %s%s"
    (get_flags src ["gcc";"compile"])
    src

let ocamlyacc src target =
  Printf.sprintf "ocamlyacc %s%s"
    (get_flags src ["ocamlyacc"])
    src

let ocamllex src target =
  Printf.sprintf "ocamllex %s%s"
    (get_flags src ["ocamlyacc"])
    src

let cond cnd rule fn =
  if cnd (tags_of_file fn) then rule fn else None

let () = build_rules := [
  make ".cmi" ".mli" ocamlc;
  cond (!? "opt") (make ".cmi" ".ml" ocamlopt);
  make ".cmi" ".ml" ocamlc;
  make ".cmo" ".ml" ocamlc;
  make ".cmx" ".ml" ocamlopt;
  
  make ".ml" ".mly" ocamlyacc;
  make ".mli" ".mly" ocamlyacc;
  make ".ml" ".mll" ocamllex;
  make ".byte" ".cmo" ocamlc_link;
  make ".opt" ".cmx" ocamlopt_link;
  make ".o" ".ml" ocamlopt;
  make ".o" ".c" gcc;

  cond (!? "pack") (fun fn -> 
		      if Filename.check_suffix fn ".cmo" 
		      then Some (ocamlc_pack fn)
		      else None);
  cond (!? "pack") (fun fn -> 
		      if Filename.check_suffix fn ".cmi" 
		      then Some (ocamlc_pack (Filename.chop_suffix fn ".cmi" ^ ".cmo"))
		      else None);
  cond (!? "pack") (fun fn -> 
		      if Filename.check_suffix fn ".cmx" 
		      then Some (ocamlopt_pack fn)
		      else None);
  cond (!? "pack") (fun fn -> 
		      if Filename.check_suffix fn ".o" 
		      then Some (ocamlopt_pack (Filename.chop_suffix fn ".o" ^ ".cmx"))
		      else None);
]

let compile glob =
  let buf = Buffer.create 16 in
  String.iter
    (function
       | '.' -> Buffer.add_string buf "\\."
       | '*' -> Buffer.add_string buf ".*"
       | '|' -> Buffer.add_string buf "\\|"
       | '(' -> Buffer.add_string buf "\\("
       | ')' -> Buffer.add_string buf "\\)"
       | c -> Buffer.add_char buf c
    )
    glob;
  let re = Str.regexp ("^" ^ (Buffer.contents buf) ^ "$") in
  fun fn -> Str.string_match re fn 0
  

let if_file glob =
  let test = compile glob in
  fun c tags f -> if test f then c tags f else tags

let set_tags ts tags f = List.fold_left (+=) tags ts

let pack target contents =
  files_rules := [
    if_file (target ^ ".(cmo|cmx|cmi|o)") 
      (set_tags ["pack";"pack_"^target]);
    if_file ("(" ^ (String.concat "|" contents) ^ ").ml") 
      (set_tags ["forpack_" ^ target])
  ] @ !files_rules;
  flags_rules := [
  ["pack_"^target;"ocaml";"byte"], 
    List.map (fun f -> f ^ ".cmo") contents;
  ["pack_"^target;"ocaml";"opt"], 
    List.map (fun f -> f ^ ".cmx") contents;
  ["forpack_"^target;"ocaml";"opt";"compile"],
    ["-for-pack " ^ String.capitalize target]
  ] @ !flags_rules

let () = 
  pack "x" ["parser";"lexer";"calc"];
  pack "y" ["sub/a";"sub/b"]


let () = main ()
