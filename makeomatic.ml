type cmd = 
    [ `Compile of string
    | `Pack of string * string
    | `Link of string * string ]

module type S = sig
  val packs: (string * string list) list
  val execs: (string * string list) list
  val ocaml: cmd -> native:bool -> string -> string
  val gcc: string -> string

  val rules: (string -> string option) list
end

let wrapper_so =
  Sys.getenv "HOME" ^ "/makeomatic/makeomatic_wrapper.so"

let cwd = Sys.getcwd ()


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
  let trace = ref false
  let fast = ref false

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

	"--fast", Set fast,
	" Fast mode";
      ]

  let usage = "Usage:\n  makeomatic <options> [target | var=val ...]\n"

  let () =
    parse specs
	(fun s -> 
	   try
	     let eq = String.index s '=' in
	     vars := (String.sub s 0 eq, 
		      String.sub s (eq+1) (String.length s - eq - 1)) :: !vars;
	   with Not_found ->
	     targets := s :: !targets) usage

  let targets = List.rev !targets
end

(* Persistent cache *)

let dir_parsed = Hashtbl.create 1

let add_file_exists f =
(*  Printf.eprintf "add_file: %s\n" f; flush stderr; *)
  let d = Filename.dirname f in
  let b = Filename.basename f in
  try
    let files = Hashtbl.find dir_parsed d in
    Hashtbl.replace files b ()
  with Not_found -> ()

let file_exists f =
  let d = Filename.dirname f in
  let b = Filename.basename f in
  try
    let files = Hashtbl.find dir_parsed d in
    Hashtbl.mem files b
  with Not_found ->
(*    Printf.eprintf "parsing dir %s for %s\n" d f; flush stderr;   *)
    let files = Hashtbl.create 1 in
    Array.iter (fun f -> Hashtbl.add files f ()) 
      (try Sys.readdir d with Sys_error _ -> [||]);
    Hashtbl.replace dir_parsed d files;
    Hashtbl.mem files b

let notify_unlink f =
(*  Printf.eprintf "unlink: %s\n" f; flush stderr;  *)
  let d = Filename.dirname f in
  let b = Filename.basename f in
  try
    let files = Hashtbl.find dir_parsed d in
    Hashtbl.remove files b
  with Not_found -> ()

let remove f =
  Sys.remove f;
  notify_unlink f
    



let cache_file = !CmdLine.cache_file

let file_cache, digest_cache as cache_state =
  if (not !CmdLine.clear_cache) && file_exists cache_file then
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

let digest_checked = Hashtbl.create 1

let digest fn =
  try Hashtbl.find digest_checked fn
  with Not_found ->
(*    Format.eprintf "stat: %s\n" fn; flush stderr; *)
    let st = Unix.stat fn in
    let info = (st.Unix.st_mtime, st.Unix.st_ino) in
    let md5 = try 
      let (md5,info') = Hashtbl.find digest_cache fn in
      if info <> info' then raise Not_found;
      md5
    with Not_found -> 
      let md5 = Digest.file fn in
      if !CmdLine.trace then Format.eprintf "MD5(%s)@." fn; 
      Hashtbl.replace digest_cache fn (md5,info); 
      md5 in
    Hashtbl.replace digest_checked fn md5;
    md5

let force_digest fn = 
  Hashtbl.remove digest_cache fn;
  digest fn



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

let extract_cmd wd s =
  match test_prefix s "##[MAKEOMATIC]## " with None -> None | Some cmd0 ->
    let cmd,arg = extract_word cmd0 in
    let adapt fn =
      if not (Filename.is_relative fn) || cwd = wd then fn
      else match test_prefix wd (cwd ^ "/") with
	| Some extra ->
	    let rec fix current f =
	      match test_prefix f "../" with
		| Some f -> 
		    if f = "." || f = "" then 
		      failwith "** file above build dir"
		    else
		      fix (Filename.dirname current) f
		| None -> 
		    if current = "." then f 
		    else if f = "." then current 
		    else Filename.concat current f
	    in
	    fix extra fn
	| None -> failwith "** cwd out of build dir..."
    in
(*
    let adapt fn =
      let f = adapt fn in
      if f <> fn then (Printf.eprintf "%s ---> %s\n" fn f; flush stderr);
      f
    in
*)
    match cmd with
      | "__xstat64" | "__fxstat64" | "__lxstat64" ->
	  let arg = adapt arg in
	  if ign arg then Some `Ignore
	  else Some (`Check arg)
      | "access" -> 
	  let _,arg = extract_word arg in
	  let arg = adapt arg in
	  if ign arg then Some `Ignore
	  else Some (`Check arg)
      | "open64" | "open" ->
	  let flags,arg = extract_word arg in
	  let arg = adapt arg in
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
	  let arg = adapt arg in
	  if ign arg then Some `Ignore
	  else (match mode with
	     | "w" | "w+" | "w+b" -> Some (`Out arg)
	     | "r" | "rb" -> Some (`In arg)
	     | _ -> Some (`Other (arg,cmd0))
	  )
      | "unlink" ->
	  let arg = adapt arg in
	  notify_unlink arg;
	  Some `Ignore
      | "symlink" ->
	  let p1,p2 = extract_word arg in
	  let p1 = adapt p1 and p2 = adapt p2 in
	  add_file_exists p2;
	  Some `Ignore
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
  let fifo_cmd_name = Printf.sprintf "%s/CMD%i" cwd !fifo_num in
  let fifo_ping_name = Printf.sprintf "%s/PING%i" cwd !fifo_num in

  Unix.mkfifo fifo_cmd_name 0o777;
  Unix.mkfifo fifo_ping_name 0o777;
  at_exit (fun () -> 
	     Sys.remove fifo_cmd_name; Sys.remove fifo_ping_name);

  Unix.putenv "MAKEOMATIC_FIFO_CMD" fifo_cmd_name;
  Unix.putenv "MAKEOMATIC_FIFO_PING" fifo_ping_name;

  incr depth;
  let pid = run_background cmdline in
  let cmd = open_in fifo_cmd_name in
  let ping = open_out fifo_ping_name in

  (try while true do 
     let s = input_line cmd in
     let cwd = input_line cmd in
     if !CmdLine.trace then print_endline s;
     match extract_cmd cwd s with
       | None -> print_endline s
       | Some c -> 
	   callback c;
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
  if fn.[0] = '/' then file_exists fn
  else if Hashtbl.mem already_built fn then file_exists fn 
  else let () = Hashtbl.add already_built fn () in
  if !CmdLine.trace then Format.eprintf "BUILD(%s)@." fn; 
  (match cmdline_for_file fn with
     | None -> ()
     | Some cmdline ->
	 if need_to_run fn cmdline then interactive fn cmdline
(*	 else Format.eprintf "%s <- (%s)@." fn cmdline*));
  if not (file_exists fn) then (Hashtbl.add absents fn (); false)
  else true
      
and cmdline_for_file fn =
(*
  match
    List.flatten
      (List.map 
	 (fun rule -> match rule fn with Some r -> [r] | None -> [])
	 !build_rules)
  with
    | [] -> None
    | [hd] -> Some hd
    | hd::_ as l ->
	Format.eprintf "* multiple rules for %s:@." fn;
	List.iter (fun r -> Format.eprintf "   + %s@." r) l; 
	Some hd
*)
  if !CmdLine.fast && (Hashtbl.mem file_cache fn || file_exists fn) then
    if Hashtbl.mem file_cache fn then
      let (cmds,_) = Hashtbl.find file_cache fn in
      let cmd = fst (List.hd cmds) in
      Some cmd
    else
      None
  else
    
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
  | `File (fn, `Absent) -> !CmdLine.fast || not (build fn)
  | `File (fn, `Digest md5) -> build fn && digest fn = md5
  | `File (fn, `Present) -> !CmdLine.fast || build fn

and check_deps l = List.for_all check_dep l

and need_to_run fout cmdline =
  if Hashtbl.mem already_run cmdline then false
  else try
    let (cmds,md5) = Hashtbl.find file_cache fout in
    if (not (file_exists fout)) || (md5 <> digest fout) then
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
	if fn = fout then ()  (* special case for "make" *)
	else
	  if not (List.mem fn !outputs) && not (Hashtbl.mem deps fn)
	  then reg fn (build_present fn)
    | `Out fn ->
	add_file_exists fn;
	Hashtbl.remove digest_checked fn;
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
  let outputs = if List.mem fout !outputs then !outputs else fout :: !outputs in
  let outputs = List.filter file_exists outputs in
  if not (List.mem fout outputs) then (
    Printf.eprintf 
      "** The file %s should have been produced by this command\n"
      fout
    ;
    exit 2
  );
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
      (fun file _ -> if file_exists file then remove file)
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
  Unix.putenv "LD_PRELOAD" wrapper_so;
  List.iter 
    (fun fn -> 
       if not (build fn) then 
	 Format.eprintf "** target %s cannot be built@." fn
    )
    CmdLine.targets;
  save_cache ()

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

let gen ext1l ext2 cmd target =
  let rec aux = function
    | [] -> None
    | ext1 :: rest ->
	match ext target ext1 ext2 with 
	  | None -> aux rest
	  | Some src -> if build src then Some (cmd src target) else None
  in aux ext1l

let from fout fins f fn =
  if fn = fout && List.for_all build fins then Some (f fins fout) else None

let concat args =
  String.concat " " (List.filter (fun s -> s <> "") args)

let compile_glob glob =
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

let ppopt s =
  let b = Buffer.create 16 in
  String.iter
    (function
       | ' ' -> Buffer.add_string b " -ppopt "
       | c -> Buffer.add_char b c
    ) (" " ^ s);
  Buffer.contents b

(****************************************************************************)


module Make(X : S) = struct

  open X

let is_pack fn = List.mem_assoc (Filename.chop_extension fn) packs

(* Transitive closure for OCaml objects *)

let ocaml_closure obj fns =
  let needed = ref [] in
  let seen = ref [] in
  let rec aux fn =
    if List.mem fn !needed then ()
    else if build fn then (
      if List.mem fn !seen then 
	(Printf.eprintf 
	   "** Circular dependency when linking %s. Aborting.\n" fn;
	 save_cache ();
	 exit 2
	);
      seen := fn :: !seen;
      if not (is_pack fn) then
	List.iter
	  (fun f ->
	     match ext f ".cmi" obj with 
	       | None -> ()
	       | Some f -> if f <> fn then aux f)
	  (depends fn);
      needed := fn :: !needed
    )
  in
  List.iter aux fns;
  String.concat " " (List.rev !needed)

let ocaml_pack obj fn =
  try
    if not (Filename.check_suffix fn obj || Filename.check_suffix fn ".cmi")
    then raise Not_found;

    let m = Filename.chop_extension fn in
    let comps = List.assoc m packs in
    let comps = 
      List.map 
	(fun s -> 
	   let f = s ^ obj in if build f then f else
	     let f = s ^ ".cmi" in if build f then f else
	       (Printf.eprintf "** Cannot build module %s needed to pack %s\n"
		  f m;
		save_cache ();
		exit 2)
	) comps in
    Some (m ^ obj, String.concat " " comps)
  with Not_found ->
    None

let ocaml_link execs obj fn =
  try 
    let comps = List.assoc fn execs in
    let comps = List.map (fun f -> f ^ obj) comps in
    Some (ocaml_closure obj comps)
  with Not_found ->
    None

(* Description of the packing structure *)

let modname fn =
  String.capitalize (Filename.basename fn)

let mk_forpacks () =
  let container = Hashtbl.create 1 in
  List.iter 
    (fun (m,comps) -> 
       List.iter (fun c -> 
		    if Hashtbl.mem container c then (
		      Printf.eprintf "** Module %s packed several times!\n" c;
		      exit 2);
		    Hashtbl.add container c m) 
	 comps) packs;
  fun f ->
    if Filename.check_suffix f ".mli" then ""
    else
      let rec aux m l =
	try 
	  let p = Hashtbl.find container m in 
	  aux p (modname p :: l)
	with Not_found -> l in
      match aux (Filename.chop_extension f) [] with
	| [] -> ""
	| l -> Printf.sprintf "-for-pack %s " (String.concat "." l)
	      
let forpack = mk_forpacks ()

let cond cnd rule = if cnd then rule else fun fn -> None
let has v = List.mem_assoc v !CmdLine.vars

let ocamlc src _ = ocaml (`Compile src) false ""
let ocamlopt src _ = ocaml (`Compile src) true (forpack src)
let ocamlc_pack fn = match ocaml_pack ".cmo" fn with
  | Some (target, comps) -> Some (ocaml (`Pack (target,comps)) false "")
  | None -> None
let ocamlopt_pack fn = match ocaml_pack ".cmx" fn with
  | Some (target, comps) -> 
      Some (ocaml (`Pack (target,comps)) true (forpack target))
  | None -> None
let ocamllex src _ =  Printf.sprintf "ocamllex %s" src
let ocamlyacc src _ =  Printf.sprintf "ocamlyacc %s" src
let gcc src _ = X.gcc src

let ocaml_exec targ =
  if Filename.check_suffix targ ".opt" 
  then match ocaml_link execs ".cmx" (Filename.chop_suffix targ ".opt") with
    | Some objs -> Some (ocaml (`Link (targ,objs)) true "")
    | None -> None
  else if Filename.check_suffix targ ".byte"
  then match ocaml_link execs ".cmo" (Filename.chop_suffix targ ".byte") with
    | Some objs -> Some (ocaml (`Link (targ,objs)) false "")
    | None -> None
  else None

let ocamlc_link src targ =
  ocaml (`Link (targ,ocaml_closure ".cmo" [src])) false ""

let ocamlopt_link src targ =
  ocaml (`Link (targ,ocaml_closure ".cmx" [src])) true ""

let () = build_rules := rules @ [
  (* ocamlc, ocamlopt: compilation *)
  gen [".cmi"] ".mli" ocamlc;
  gen [".cmi";".cmo"] ".ml" ocamlc;
  gen [".cmx";".o"] ".ml" ocamlopt;

  (* ocamlc, ocamlopt: link *)
  gen [".byte"] ".cmo" ocamlc_link;
  gen [".opt"] ".cmx" ocamlopt_link;

  ocamlc_pack;
  ocamlopt_pack;
  ocaml_exec;
    
  (* ocamllex, ocamlyacc *)
  gen [".ml";".mli"] ".mly" ocamlyacc;
  gen [".ml"] ".mll" ocamllex;

  (* gc *)
  gen [".o"] ".c" gcc;

]


let () = 
  Sys.catch_break true;
  main ()

end
